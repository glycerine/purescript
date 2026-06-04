export const internalError = function(msg) {
  throw new Error("An internal error occurred during compilation: " + msg + "\nPlease report this at https://github.com/purescript/purescript/issues");
};
