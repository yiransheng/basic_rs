pub trait KeywordToken: Copy {
    /// Parses from input string, returns (number of characters matched, value)
    /// tuple if successful; None otherwise.
    fn parse_from_str(s: &str) -> Option<(usize, Self)>
    where
        Self: Sized;
}
