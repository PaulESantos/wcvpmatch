# #' Install Rust and rustup on the System
# #'
# #' Downloads and installs Rust via \code{rustup}, the official Rust toolchain
# #' installer. On Windows, the installer binary is downloaded from
# #' \url{https://win.rustup.rs/x86_64}. On Unix-like systems (Linux and macOS),
# #' installation is performed through the \code{rustup.rs} shell script.
# #' After installation, \code{~/.cargo/bin} is added to the \code{PATH} of the
# #' current R session.
# #'
# #' @param browse Logical. If \code{TRUE}, opens the official Rust installation
# #'   page (\url{https://www.rust-lang.org/tools/install}) in the default
# #'   browser before proceeding. Defaults to \code{FALSE}.
# #' @param quiet Logical. If \code{TRUE}, suppresses progress messages from the
# #'   file download step. Installer output may still appear depending on the
# #'   platform. Defaults to \code{FALSE}.
# #'
# #' @return Invisibly returns \code{TRUE} if Rust was already present or was
# #'   installed successfully, and \code{FALSE} if the installation failed.
# #'
# #' @details
# #' The function first checks whether \code{rustup} is already available on the
# #' system \code{PATH}. If found, it reports the installed version and exits
# #' early without making any changes.
# #'
# #' On Windows, the \code{stable} toolchain is installed with the
# #' \code{--profile minimal} flag to reduce download size. On Unix-like systems,
# #' the same profile is requested via the \code{rustup.rs} bootstrap script,
# #' which requires \code{curl} to be available.
# #'
# #' A restart of the R session may be required for the updated \code{PATH} to
# #' take full effect in all contexts (e.g., when building packages with
# #' \code{\link[pkgbuild]{pkgbuild}}).
# #'
# #' @note
# #' This function requires an active internet connection. On Windows, the
# #' installer is run silently (\code{-y}) and sets the \code{stable} toolchain
# #' as the default. Administrator privileges are not required.
# #'
# #' @seealso
# #' \code{\link{set.rust.toolchain.gnu}} for configuring the GNU toolchain on
# #' Windows, which is required when building R packages with Rtools.
# #'
# #' @examples
# #' \dontrun{
# #' # Install Rust silently
# #' install.Rust()
# #'
# #' # Open the official page and then install
# #' install.Rust(browse = TRUE)
# #' }
# #'
# #' @export
# install.Rust <- function(browse = FALSE, quiet = FALSE) {
#
#   if (browse) utils::browseURL("https://www.rust-lang.org/tools/install")
#
#   # Already installed? Report version and exit early
#   if (nchar(Sys.which("rustup")) > 0) {
#     ver <- system("rustup --version", intern = TRUE)
#     message("Rust is already installed: ", ver)
#     return(invisible(TRUE))
#   }
#
#   os <- .Platform$OS.type
#
#   # Windows installation path
#   if (os == "windows") {
#     url      <- "https://win.rustup.rs/x86_64"
#     destfile <- file.path(tempdir(), "rustup-init.exe")
#
#     message("Downloading rustup-init.exe ...")
#     utils::download.file(url, destfile, mode = "wb", quiet = quiet)
#
#     message("Installing Rust (silent mode) ...")
#     exit_code <- shell(
#       paste(destfile, "-y --default-toolchain stable --profile minimal"),
#       wait = TRUE
#     )
#
#     if (exit_code != 0) {
#       warning("Rust installation failed (exit code: ", exit_code, ").")
#       return(invisible(FALSE))
#     }
#
#     # Add ~/.cargo/bin to PATH for the current R session
#     cargo_bin <- file.path(Sys.getenv("USERPROFILE"), ".cargo", "bin")
#     Sys.setenv(PATH = paste(cargo_bin, Sys.getenv("PATH"), sep = ";"))
#
#     # Unix-like installation path (Linux / macOS)
#   } else {
#     message("Installing Rust via rustup.rs ...")
#     system(
#       "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --profile minimal"
#     )
#
#     # Add ~/.cargo/bin to PATH for the current R session
#     cargo_bin <- file.path(Sys.getenv("HOME"), ".cargo", "bin")
#     Sys.setenv(PATH = paste(cargo_bin, Sys.getenv("PATH"), sep = ":"))
#   }
#
#   message("Rust installed successfully. Restarting the R session may be required.")
#   invisible(TRUE)
# }
#
#
# #' Configure the GNU Rust Toolchain on Windows
# #'
# #' Installs and sets the \code{stable-x86_64-pc-windows-gnu} toolchain as the
# #' default Rust toolchain on Windows. This configuration is required when
# #' building R packages that include Rust code, since R on Windows uses the
# #' GNU ABI (provided by Rtools) rather than the MSVC ABI.
# #'
# #' @param install Logical. If \code{TRUE} (default), the GNU toolchain is
# #'   downloaded and installed automatically when not already present. If
# #'   \code{FALSE}, the function returns \code{FALSE} with a message instead of
# #'   installing.
# #'
# #' @return Invisibly returns \code{TRUE} if the toolchain was configured
# #'   successfully, or \code{FALSE} if the function exited early (non-Windows
# #'   system, \code{rustup} not found, or \code{install = FALSE} with a missing
# #'   toolchain).
# #'
# #' @details
# #' The function performs the following steps in order:
# #' \enumerate{
# #'   \item Ensures \code{~/.cargo/bin} is present in the current session's
# #'     \code{PATH}, reconstructing it if necessary after an R session restart.
# #'   \item Verifies that \code{rustup} is accessible, stopping with an
# #'     informative error if it is not.
# #'   \item Installs the \code{stable-x86_64-pc-windows-gnu} toolchain if it is
# #'     not already listed by \code{rustup toolchain list}.
# #'   \item Sets the GNU toolchain as the \emph{global} default via
# #'     \code{rustup default}. A directory-level override is intentionally
# #'     avoided because R package compilation occurs in a temporary directory
# #'     where overrides would have no effect.
# #'   \item Adds the \code{x86_64-pc-windows-gnu} target (standard library
# #'     components) to the toolchain. Without this step, Cargo fails with a
# #'     \emph{"can't find crate for `core`"} error.
# #'   \item Checks for the presence of \code{gcc} on the \code{PATH} and emits
# #'     a warning with a link to Rtools if it is not found.
# #' }
# #'
# #' @note
# #' This function is only relevant on Windows and returns \code{FALSE}
# #' immediately on other platforms. \code{\link{install.Rust}} must be run
# #' before calling this function.
# #'
# #' @seealso
# #' \code{\link{install.Rust}} for installing Rust and \code{rustup}.
# #' Rtools for Windows: \url{https://cran.r-project.org/bin/windows/Rtools/}
# #'
# #' @examples
# #' \dontrun{
# #' # Install Rust first (if not already installed)
# #' install.Rust()
# #'
# #' # Then configure the GNU toolchain required for R package compilation
# #' set.rust.toolchain.gnu()
# #'
# #' # Check without installing if the toolchain is missing
# #' set.rust.toolchain.gnu(install = FALSE)
# #' }
# #'
# #' @export
# set.rust.toolchain.gnu <- function(install = TRUE) {
#
#   if (.Platform$OS.type != "windows") {
#     message("This function is only required on Windows.")
#     return(invisible(FALSE))
#   }
#
#   # Normalize paths before comparing to avoid false mismatches on Windows
#   # (backslash vs forward slash, drive letter casing, etc.)
#   cargo_bin      <- file.path(Sys.getenv("USERPROFILE"), ".cargo", "bin")
#   cargo_bin_norm <- normalizePath(cargo_bin, winslash = "/", mustWork = FALSE)
#   path_norm      <- normalizePath(
#     strsplit(Sys.getenv("PATH"), ";")[[1]],
#     winslash  = "/",
#     mustWork  = FALSE
#   )
#
#   if (dir.exists(cargo_bin) && !cargo_bin_norm %in% path_norm) {
#     message("Adding ~/.cargo/bin to the current session PATH ...")
#     Sys.setenv(PATH = paste(cargo_bin, Sys.getenv("PATH"), sep = ";"))
#   }
#
#   if (nchar(Sys.which("rustup")) == 0) {
#     stop("rustup not found at: ", cargo_bin, "\nRun install.Rust() first.")
#   }
#
#   target     <- "stable-x86_64-pc-windows-gnu"
#   toolchains <- system("rustup toolchain list", intern = TRUE)
#
#   # Early exit: check active toolchain directly — more reliable than
#   # parsing the (default) marker from `rustup toolchain list`
#   active_toolchain <- tryCatch(
#     system("rustup show active-toolchain", intern = TRUE)[1],
#     error = function(e) ""
#   )
#
#   if (grepl(target, active_toolchain, fixed = TRUE)) {
#     message("Toolchain '", target, "' is already set as default. No changes needed.")
#
#     if (nchar(Sys.which("gcc")) == 0) {
#       warning(
#         "GCC not detected on the PATH.\n",
#         "Install Rtools from: https://cran.r-project.org/bin/windows/Rtools/"
#       )
#     } else {
#       message("GCC detected: ", system("gcc --version", intern = TRUE)[1])
#     }
#
#     return(invisible(TRUE))
#   }
#
#   # Step 1: Install the GNU toolchain if not already present
#   if (!any(grepl(target, toolchains))) {
#     if (!install) {
#       message("Toolchain '", target, "' is not installed. Use install = TRUE.")
#       return(invisible(FALSE))
#     }
#     message("Installing toolchain: ", target, " ...")
#     system(paste("rustup install", target))
#   }
#
#   # Step 2: Set GNU as the global default toolchain (not just a directory override).
#   # A directory-level override only applies to the current working directory.
#   # Since R compiles packages in a temporary directory, a global default is required.
#   message("Setting '", target, "' as the default toolchain ...")
#   system(paste("rustup default", target))
#
#   # Step 3: Add the GNU target (core + std libraries) to the toolchain.
#   # The toolchain and the target are distinct components in Rust.
#   # Without this step, Cargo fails with "can't find crate for `core`".
#   message("Adding target x86_64-pc-windows-gnu ...")
#   system(paste("rustup target add x86_64-pc-windows-gnu --toolchain", target))
#
#   # Step 4: Verify that GCC is available via Rtools
#   if (nchar(Sys.which("gcc")) == 0) {
#     warning(
#       "GCC not detected on the PATH.\n",
#       "Install Rtools from: https://cran.r-project.org/bin/windows/Rtools/"
#     )
#   } else {
#     message("GCC detected: ", system("gcc --version", intern = TRUE)[1])
#   }
#
#   message("GNU toolchain configured successfully.")
#   invisible(TRUE)
# }
#
