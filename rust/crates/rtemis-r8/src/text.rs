//! Text that every interface of R8 prints the same: the size of a value's
//! memory.
//! spec: R8/class-system#c-interface

/// A size in bytes in IEC binary units, `B`, `KiB`, `MiB`, `GiB`, and
/// `TiB`: the exact count below 1 KiB (`120 B`), and otherwise the size in
/// the largest unit it reaches, to three significant digits (`7.63 MiB`,
/// `76.3 MiB`, `763 MiB`, `1020 KiB`).
pub fn format_size(bytes: u64) -> String {
    const UNITS: [&str; 4] = ["KiB", "MiB", "GiB", "TiB"];
    if bytes < 1024 {
        return format!("{bytes} B");
    }
    let mut value = bytes as f64 / 1024.0;
    let mut unit = 0;
    while value >= 1024.0 && unit < UNITS.len() - 1 {
        value /= 1024.0;
        unit += 1;
    }
    // Three significant digits: round at the third digit, then show as many
    // decimals as the rounded value leaves to it.
    let magnitude = value.log10().floor() as i32;
    let scale = 10f64.powi(magnitude - 2);
    let rounded = (value / scale).round() * scale;
    let decimals = (2 - rounded.log10().floor() as i32).max(0) as usize;
    format!("{rounded:.decimals$} {}", UNITS[unit])
}

#[cfg(test)]
mod tests {
    use super::format_size;

    #[test]
    fn sizes_take_iec_units_and_three_significant_digits() {
        assert_eq!(format_size(0), "0 B");
        assert_eq!(format_size(120), "120 B");
        assert_eq!(format_size(1023), "1023 B");
        assert_eq!(format_size(1024), "1.00 KiB");
        assert_eq!(format_size(1536), "1.50 KiB");
        assert_eq!(format_size(8_000_048), "7.63 MiB");
        assert_eq!(format_size(80_000_048), "76.3 MiB");
        assert_eq!(format_size(800_000_048), "763 MiB");
        assert_eq!(format_size(1023 * 1024 + 900), "1020 KiB");
        assert_eq!(format_size(10_239), "10.0 KiB");
        assert_eq!(format_size(5 * 1024u64.pow(4)), "5.00 TiB");
        assert_eq!(format_size(2000 * 1024u64.pow(4)), "2000 TiB");
    }
}
