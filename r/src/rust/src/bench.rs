//! The probes of `r8bench()`, over `rtemis_r8::resources`: the CPU time and
//! the memory of R's process, a thread that samples its memory, the cores
//! that it may use, and the line that the core words for a benchmark.
//!
//! spec: R8/raven#benchmarks

use std::sync::{Mutex, OnceLock};
use std::time::{Duration, Instant};

use extendr_api::prelude::*;
use rtemis_r8::message::{Engine, bench_ended};
use rtemis_r8::resources::{Sampler, cores_available, usage};

/// A count of bytes as a double, `NA` where the system gives none.
fn bytes(n: Option<u64>) -> Rfloat {
    n.map_or(Rfloat::na(), |n| Rfloat::from(n as f64))
}

/// The CPU time and the memory of R's process.
///
/// @return A list of `user` and `system`, the seconds of CPU time of all its
///   threads, `rss`, the bytes it holds, and `peak`, the most it has held, the
///   operating system's high-water mark; a figure that the system does not
///   give is `NA`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_usage() -> List {
    let u = usage();
    list!(
        user = u.user,
        system = u.system,
        rss = bytes(u.rss),
        peak = bytes(u.peak)
    )
}

/// Seconds on a monotonic clock, from the first read in the process: the
/// clock of the times of a plan's stages in R, which match the core's.
///
/// @return Double.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_clock() -> f64 {
    static ORIGIN: OnceLock<Instant> = OnceLock::new();
    ORIGIN.get_or_init(Instant::now).elapsed().as_secs_f64()
}

/// A sampler of the memory of R's process, which holds its thread.
struct Held(Mutex<Option<Sampler>>);

/// Starts a thread that reads the memory of R's process every `every_ms`
/// milliseconds; it calls no function of R.
///
/// @param every_ms Integer: The interval.
/// @return An external pointer to the sampler, for `r8_sampler_stop()`.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_sampler_start(every_ms: i32) -> ExternalPtr<Held> {
    let every = Duration::from_millis(u64::try_from(every_ms).unwrap_or(10).max(1));
    ExternalPtr::new(Held(Mutex::new(Some(Sampler::start(every)))))
}

/// Stops a sampler and gives the most memory that it read.
///
/// @param sampler The external pointer that `r8_sampler_start()` gives.
/// @return Double: The bytes, or `NA` where the system gives no reading or
///   the sampler has stopped.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_sampler_stop(sampler: ExternalPtr<Held>) -> Rfloat {
    let taken = sampler.0.lock().ok().and_then(|mut s| s.take());
    bytes(taken.and_then(Sampler::stop))
}

/// The cores that R's process may use: those that the operating system gives
/// it, and at most `SLURM_CPUS_PER_TASK` in a Slurm job.
///
/// @return Integer.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_cores_available() -> i32 {
    i32::try_from(cores_available()).unwrap_or(i32::MAX)
}

/// The line of a benchmark's result, in the core's words.
///
/// @param wall,cpu Double: The seconds of wall and CPU time.
/// @param threads Integer: The threads that R8's work may use.
/// @param peak Double: The most bytes that the process held, or `NA`.
/// @param engine Character: `"rust"`, `"r"`, `"rust+r"`, or `""` where the
///   expression ran no table function of R8.
/// @return Character.
/// @keywords internal
/// @noRd
#[extendr]
fn r8_bench_line(wall: f64, cpu: f64, threads: i32, peak: f64, engine: &str) -> String {
    let engine = match engine {
        "rust" => Some(Engine::Rust),
        "r" => Some(Engine::Host),
        "rust+r" => Some(Engine::Both),
        _ => None,
    };
    // `NA` is a `NaN` of R.
    let peak = (!peak.is_nan()).then(|| peak.max(0.0) as u64);
    bench_ended(
        wall,
        cpu,
        u64::try_from(threads).unwrap_or(1).max(1),
        peak,
        engine,
    )
}

extendr_module! {
    mod bench;
    fn r8_clock;
    fn r8_usage;
    fn r8_sampler_start;
    fn r8_sampler_stop;
    fn r8_cores_available;
    fn r8_bench_line;
}
