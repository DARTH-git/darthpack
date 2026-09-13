#' Open UserGuide of the package
#'
#' \code{open_guide} opens the bookdown user guide of the framework. The guide
#' is built from the \emph{report} directory of the \code{darthpack}
#' repository, so it is available when the repository is used as a coding
#' template (for instance after \code{devtools::load_all(".")}). When
#' \code{darthpack} is used as an installed package the guide is not part of
#' the installation, and this function opens the online version instead.
#'
#' @param ext Extension of the book to open: 'html' or 'pdf'
#' @param online Logical variable to open the online version of the guide
#' rather than looking for a local copy. Default = FALSE
#'
#' @return
#' Invisibly, the URL or path that was opened.
#'
#' @importFrom utils browseURL
#'
#' @examples
#' \dontrun{
#'   open_guide()
#'   open_guide("pdf")
#' }
#'
#' @export
open_guide <- function(ext = "html", online = FALSE) {
  v_ext_valid <- c("html", "pdf")
  if (length(ext) != 1 || !(ext %in% v_ext_valid)) {
    stop("'ext' must be one of: ", paste(v_ext_valid, collapse = ", "))
  }

  url_online <- "https://darth-git.github.io/darthpack/articles/aa-introduction.html"

  if (online) {
    browseURL(url_online)
    return(invisible(url_online))
  }

  guide_file <- if (ext == "html") "report/_book/index.html" else "report/_book/report.pdf"
  guide_path <- system.file(guide_file, package = "darthpack")

  # system.file() returns "" when the file is not part of the installation. The
  # previous version pasted that empty string onto "file:/" and opened the root
  # of the filesystem instead of the guide.
  if (!nzchar(guide_path) || !file.exists(guide_path)) {
    message("The local user guide was not found (it is built from the ",
            "'report' directory of the darthpack repository, which is not ",
            "part of the installed package). Opening the online version.")
    browseURL(url_online)
    return(invisible(url_online))
  }

  browseURL(paste0("file://", normalizePath(guide_path)))
  return(invisible(guide_path))
}
