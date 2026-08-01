use oxc_str::Ident;
use oxc_syntax::reference::ReferenceId;

/// Names of the references created during AST traversal, indexed by [`ReferenceId`].
///
/// Instead of maintaining per-scope hashmaps and merging them on scope exit (bubble-up),
/// the name of every reference is recorded flat and references are resolved in a single pass
/// after traversal (walk-up). This eliminates all hashmap drain+insert operations during
/// scope exit.
///
/// Every reference created while building [`Semantic`] goes through
/// [`SemanticBuilder::declare_reference`], which pushes exactly one name here, so entry `i`
/// always describes `ReferenceId(i)`. That 1:1 correspondence means the [`ReferenceId`] does
/// not have to be stored alongside the name (which would cost a further 8 bytes per reference
/// once padding is accounted for), and "already resolved" is read from the [`Reference`] itself
/// rather than tracked by compacting this list.
///
/// [`Semantic`]: crate::Semantic
/// [`SemanticBuilder::declare_reference`]: crate::SemanticBuilder::declare_reference
/// [`Reference`]: oxc_syntax::reference::Reference
pub struct ReferenceNames<'a> {
    names: Vec<Ident<'a>>,
}

impl<'a> ReferenceNames<'a> {
    pub(crate) fn new() -> Self {
        Self { names: Vec::new() }
    }

    /// Reserve exactly `additional` more slots in the underlying `Vec`.
    /// Avoids growth reallocations when the expected count is known up-front
    /// (typically from [`crate::Stats::count`]).
    #[inline]
    pub(crate) fn reserve_exact(&mut self, additional: usize) {
        self.names.reserve_exact(additional);
    }

    /// Record the name of the next reference.
    ///
    /// The caller must push exactly once per reference created, in [`ReferenceId`] order.
    #[inline]
    pub(crate) fn push(&mut self, name: Ident<'a>) {
        self.names.push(name);
    }

    /// Take all recorded names, leaving the list empty. O(1) pointer swap.
    #[inline]
    pub(crate) fn take(&mut self) -> Vec<Ident<'a>> {
        std::mem::take(&mut self.names)
    }

    /// Number of names recorded so far, which is also the number of references created so far.
    ///
    /// Used as a checkpoint for early resolution: every reference created after the checkpoint
    /// has a [`ReferenceId`] greater than or equal to it.
    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.names.len()
    }

    /// Read the name of a reference, by value.
    ///
    /// Used by [`crate::SemanticBuilder::resolve_references_for_current_scope`] to process a
    /// range of references without allocating a temporary `Vec`. `Ident<'a>` is `Copy`, so this
    /// hands the caller an owned name that's detached from the underlying borrow.
    ///
    /// # Panics
    /// Panics if `reference_id` is out of bounds.
    #[inline]
    pub(crate) fn get(&self, reference_id: ReferenceId) -> Ident<'a> {
        self.names[reference_id.index()]
    }
}
