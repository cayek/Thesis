#' LD clumping with plink
#'
#' see : http://zzz.bwh.harvard.edu/plink/clump.shtml
#' see: https://neurogenetics.qimrberghofer.edu.au/iSECA/LD_clumping_tutorial.html
#'
#' @export
Plink_LD_clumping <- function(prefix.in,
                              clump.p1 = 0.0001,
                              clump.p2 = 0.01,
                              clump.r2 = 0.50,
                              clump.kb = 250) {

  prefix.in <- path.expand(prefix.in)
  dir.name <- dirname(prefix.in)

  cmd <- paste("plink",
                "--bfile",
               prefix.in,
               "--clump-p1", clump.p1,
               "--clump-p2", clump.p2,
               "--clump-r2", clump.r2,
               "--clump-kb", clump.kb,
               "--clump", tempfile(tmpdir = dir.name))
  system(cmd)

}
