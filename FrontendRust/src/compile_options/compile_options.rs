#[derive(Clone)]
pub struct GlobalOptions {
  pub sanity_check: bool,
  pub use_overload_index: bool,
  pub use_optimized_solver: bool,
  pub verbose_errors: bool,
  pub debug_output: bool,
}

impl GlobalOptions {
  /*
  package dev.vale.options

  object GlobalOptions {
  */
  pub fn apply() -> GlobalOptions {
    GlobalOptions {
      sanity_check: false,
      use_overload_index: false,
      use_optimized_solver: true,
      verbose_errors: false,
      debug_output: false,
    }
  }
  /*
    def apply(): GlobalOptions = {
      GlobalOptions(
        sanityCheck = false,
        useOverloadIndex = false,
        useOptimizedSolver = true,
        verboseErrors = false,
        debugOutput = false)
    }
  */
  pub fn test() -> GlobalOptions {
    GlobalOptions {
      sanity_check: true,
      use_overload_index: false,
      use_optimized_solver: true,
      verbose_errors: true,
      debug_output: true,
    }
  }
  /*
    def test(): GlobalOptions = {
      GlobalOptions(true, false, true, true, true)
    }
  }
  */
}

/*
case class GlobalOptions(
  sanityCheck: Boolean,
  useOverloadIndex: Boolean,
  useOptimizedSolver: Boolean,
  verboseErrors: Boolean,
  debugOutput: Boolean)
*/
