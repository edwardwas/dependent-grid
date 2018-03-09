{compiler ? "default", profiling ? false}:
(import ./default.nix {inherit compiler profiling;}).env
