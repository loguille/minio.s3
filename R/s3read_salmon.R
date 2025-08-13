#' @rdname notdefine
#' @title readSalmon
#' @description Serialization interface to read salmon quantification file to S3
#' @author Loic Guille <loic.guille27@gmail.com>
#'
#' @param x For \code{s3read_salmon}, multiple quantification files generated using salmon to be read via \code{\link[base]{saveRDS}} and uploaded to S3. \code{x} is analogous to the code{object} argument in \code{saveRDS}.
#' @template object
#' @template bucket
#' @param compress A logical. See \code{\link[base]{saveRDS}}
#' @template dots
#'
#' @details Note that early versions of \code{s3saveRDS} from aws.s3 <= 0.2.4 unintentionally serialized objects to big endian format (due to defaults in \code{\link[base]{serialize}}. This can create problems when attempting to read these files using \code{\link[base]{readRDS}}. The function attempts to catch the issue and read accordingly, but may fail. The solution used internally is \code{unserialize(memDecompress(get_object(), "gzip"))}
#' @return For \code{s3read_salmon} a Large list
#' @examples@
#' \dontrun {
#' # create_bucket
#' b <- put_bucket("myexamplebucket")
#' 
#' # create list of files path
#' sampleinfo <- read_tsv("samplesheet.tsv", col_types = (ccc))
#' #SampleName Status       Timepoint
#' #<chr>      <chr>        <chr>
#' #SRR7457551 transplanted 24h      
#' #SRR7457552 transplanted 24h      
#'
#' files <- str_c("Bulk_experiment/results_quant/",sampleinfo$sampleName, "/quant.sf")
#' files <- set_names(files, sampleinfo$SampleName)
#' tx2gene <- read_tsv("transcript_to_gene_mm.tsv", col_types = c("cc"))
#'
#' # read multiple quantification file using tximport function
#' quant <- s3read_salmon(object = files, bucket = b,tx2gene = tx2gene)
#' }
#' @seealso \code{\link{s3save}},\code{\link{s3load}}
#' @export
s3read_salmon <- function(object, bucket, tx2gene) {
  if (missing(bucket)) {
    bucket <- get_bucket(object)
  }
  test <- c()
  for (i in 1:length(object)){
    object[i] <- get_objectkey(object[i])
    tmp <- tempfile(fileext = paste0(".",tools::file_ext(object[i])))
    tmp <- set_names(tmp,names(object[i]))
    test <- c(test,tmp)
    r <- save_object(bucket = bucket, object = object[i], file = tmp, use_https = FALSE)
  }
  print(test)
  return(tximport(test, type = "salmon", tx2gene = tx2gene))
}
