pub fn unquote_string(s: &str) -> &str {
    &s[1..s.len() - 1]
}
