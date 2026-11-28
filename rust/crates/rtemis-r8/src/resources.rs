//! The resources of the process, for a benchmark of a run: the CPU time of
//! all its threads, the memory it holds and the most it has held, a thread
//! that samples the memory it holds, and the cores that the process may use.
//! The probes read the operating system: `getrusage()` and `/proc/self/statm`
//! on Linux, `getrusage()` and `proc_pidinfo()` on macOS, and
//! `GetProcessTimes()` and `K32GetProcessMemoryInfo()` on Windows; elsewhere,
//! as on WebAssembly, each gives nothing.
//!
//! spec: R8/raven#benchmarks

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::thread::JoinHandle;
use std::time::Duration;

/// The CPU time of the process, in seconds of all its threads, and its
/// memory, in bytes.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct Usage {
    /// The seconds of CPU time in user mode.
    pub user: f64,
    /// The seconds of CPU time in the kernel.
    pub system: f64,
    /// The memory that the process holds: its resident set.
    pub rss: Option<u64>,
    /// The most memory that the process has held, the operating system's
    /// high-water mark of its resident set.
    pub peak: Option<u64>,
}

/// The CPU time and the memory of the process at the call.
pub fn usage() -> Usage {
    os::usage()
}

/// The memory that the process holds, in bytes: its resident set.
pub fn rss() -> Option<u64> {
    os::rss()
}

/// The cores that the process may use: those that the operating system
/// gives it (by its affinity and, on Linux, its cgroup's quota), and at most
/// `SLURM_CPUS_PER_TASK` where a Slurm job sets it.
pub fn cores_available() -> u64 {
    let os = std::thread::available_parallelism().map_or(1, |n| n.get() as u64);
    let slurm = std::env::var("SLURM_CPUS_PER_TASK")
        .ok()
        .and_then(|v| v.trim().parse::<u64>().ok())
        .filter(|&n| n > 0);
    slurm.map_or(os, |s| s.min(os))
}

/// A thread that reads the memory of the process at an interval and keeps
/// the most it has read; it calls no function of a host.
#[derive(Debug)]
pub struct Sampler {
    stop: Arc<AtomicBool>,
    peak: Arc<AtomicU64>,
    handle: Option<JoinHandle<()>>,
}

impl Sampler {
    /// Starts the thread, which reads the memory at its start and every
    /// `every`.
    pub fn start(every: Duration) -> Sampler {
        let stop = Arc::new(AtomicBool::new(false));
        let peak = Arc::new(AtomicU64::new(rss().unwrap_or(0)));
        let handle = {
            let (stop, peak) = (Arc::clone(&stop), Arc::clone(&peak));
            std::thread::Builder::new()
                .name("r8-sampler".into())
                .spawn(move || {
                    while !stop.load(Ordering::Relaxed) {
                        if let Some(r) = rss() {
                            peak.fetch_max(r, Ordering::Relaxed);
                        }
                        std::thread::park_timeout(every);
                    }
                })
                .ok()
        };
        Sampler { stop, peak, handle }
    }

    /// Stops the thread and gives the most memory that it read, with a last
    /// reading at the stop; `None` where the operating system gives no
    /// reading.
    pub fn stop(mut self) -> Option<u64> {
        self.halt();
        let last = rss()?;
        Some(self.peak.load(Ordering::Relaxed).max(last))
    }

    fn halt(&mut self) {
        self.stop.store(true, Ordering::Relaxed);
        if let Some(h) = self.handle.take() {
            h.thread().unpark();
            let _ = h.join();
        }
    }
}

impl Drop for Sampler {
    fn drop(&mut self) {
        self.halt();
    }
}

#[cfg(unix)]
mod os {
    use super::Usage;

    fn seconds(t: libc::timeval) -> f64 {
        t.tv_sec as f64 + t.tv_usec as f64 / 1e6
    }

    pub(super) fn usage() -> Usage {
        // SAFETY: `getrusage` fills the structure it is given.
        let mut u: libc::rusage = unsafe { std::mem::zeroed() };
        if unsafe { libc::getrusage(libc::RUSAGE_SELF, &mut u) } != 0 {
            return Usage {
                rss: rss(),
                ..Usage::default()
            };
        }
        // `ru_maxrss` is in bytes on macOS and in kibibytes on Linux.
        let scale: u64 = if cfg!(target_os = "macos") { 1 } else { 1024 };
        Usage {
            user: seconds(u.ru_utime),
            system: seconds(u.ru_stime),
            rss: rss(),
            peak: u64::try_from(u.ru_maxrss).ok().map(|m| m * scale),
        }
    }

    #[cfg(target_os = "macos")]
    pub(super) fn rss() -> Option<u64> {
        // SAFETY: `proc_pidinfo` fills at most `size` bytes of the structure.
        let mut info: libc::proc_taskinfo = unsafe { std::mem::zeroed() };
        let size = std::mem::size_of::<libc::proc_taskinfo>() as libc::c_int;
        let got = unsafe {
            libc::proc_pidinfo(
                libc::getpid(),
                libc::PROC_PIDTASKINFO,
                0,
                (&mut info as *mut libc::proc_taskinfo).cast(),
                size,
            )
        };
        (got == size).then_some(info.pti_resident_size)
    }

    #[cfg(not(target_os = "macos"))]
    pub(super) fn rss() -> Option<u64> {
        // The second field of `/proc/self/statm` is the resident pages.
        let statm = std::fs::read_to_string("/proc/self/statm").ok()?;
        let pages: u64 = statm.split_whitespace().nth(1)?.parse().ok()?;
        // SAFETY: `sysconf` reads a constant of the system.
        let page = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };
        u64::try_from(page).ok().map(|p| pages * p)
    }
}

#[cfg(windows)]
mod os {
    use super::Usage;

    #[repr(C)]
    #[derive(Default)]
    struct FileTime {
        low: u32,
        high: u32,
    }

    #[repr(C)]
    #[derive(Default)]
    struct MemoryCounters {
        cb: u32,
        page_fault_count: u32,
        peak_working_set_size: usize,
        working_set_size: usize,
        quota_peak_paged_pool_usage: usize,
        quota_paged_pool_usage: usize,
        quota_peak_non_paged_pool_usage: usize,
        quota_non_paged_pool_usage: usize,
        pagefile_usage: usize,
        peak_pagefile_usage: usize,
    }

    #[link(name = "kernel32")]
    unsafe extern "system" {
        fn GetCurrentProcess() -> isize;
        fn GetProcessTimes(
            process: isize,
            creation: *mut FileTime,
            exit: *mut FileTime,
            kernel: *mut FileTime,
            user: *mut FileTime,
        ) -> i32;
        fn K32GetProcessMemoryInfo(process: isize, counters: *mut MemoryCounters, cb: u32) -> i32;
    }

    /// Seconds from a count of 100 ns.
    fn seconds(t: &FileTime) -> f64 {
        ((u64::from(t.high) << 32) | u64::from(t.low)) as f64 / 1e7
    }

    fn memory() -> Option<MemoryCounters> {
        let mut m = MemoryCounters {
            cb: std::mem::size_of::<MemoryCounters>() as u32,
            ..MemoryCounters::default()
        };
        // SAFETY: the call fills at most `cb` bytes of the structure.
        let ok = unsafe { K32GetProcessMemoryInfo(GetCurrentProcess(), &mut m, m.cb) };
        (ok != 0).then_some(m)
    }

    pub(super) fn usage() -> Usage {
        let (mut c, mut e, mut k, mut u) = Default::default();
        // SAFETY: the call fills the four structures it is given.
        let ok = unsafe { GetProcessTimes(GetCurrentProcess(), &mut c, &mut e, &mut k, &mut u) };
        let m = memory();
        Usage {
            user: if ok != 0 { seconds(&u) } else { 0.0 },
            system: if ok != 0 { seconds(&k) } else { 0.0 },
            rss: m.as_ref().map(|m| m.working_set_size as u64),
            peak: m.as_ref().map(|m| m.peak_working_set_size as u64),
        }
    }

    pub(super) fn rss() -> Option<u64> {
        memory().map(|m| m.working_set_size as u64)
    }
}

#[cfg(not(any(unix, windows)))]
mod os {
    use super::Usage;

    pub(super) fn usage() -> Usage {
        Usage::default()
    }

    pub(super) fn rss() -> Option<u64> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_process_holds_memory_and_has_spent_time() {
        let u = usage();
        assert!(u.user + u.system >= 0.0);
        if cfg!(any(target_os = "macos", target_os = "linux", windows)) {
            let rss = u.rss.expect("a resident set");
            assert!(rss > 0);
            assert!(u.peak.expect("a high-water mark") >= rss / 2);
        }
        assert!(cores_available() >= 1);
    }

    #[test]
    fn the_sampler_keeps_the_most_memory_it_read() {
        let s = Sampler::start(Duration::from_millis(1));
        let held: Vec<u8> = vec![1; 64 << 20];
        std::thread::sleep(Duration::from_millis(30));
        let peak = s.stop();
        std::hint::black_box(&held);
        if let (Some(peak), Some(now)) = (peak, rss()) {
            assert!(peak >= now.min(64 << 20));
        }
    }
}
