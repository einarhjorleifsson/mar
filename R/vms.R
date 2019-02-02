#' VMS data
#'
#' Athugið aðgangur er takmarkaður við fáa notendur á Hafrannsóknastofnun
#'
#' @name vms
#'
#' @param con connection to mar database
#' @param t1 Start date in the format "YYYY-MM-DD"
#' @param t2 End date in the format "YYYY-MM-DD"
#'
#' @export
#'
vms <- function(con, t1, t2) {

  q <-
    tbl_mar(con, "stk.stk_vms_v") %>%
    mutate(lon = poslon * 45 / atan(1),
           lat = poslat * 45 / atan(1),
           heading = heading * 45 / atan(1),
           speed = speed * 3600/1852)
  if(!missing(t1)) {
    q <-
      q %>%
      filter(recdate >= to_date(t1, "YYYY:MM:DD"),
             recdate <=  to_date(t2, "YYYY:MM:DD"))
  }

  q %>%
    select(mobileid, vid = skip_nr, time = posdate, rectime = recdate, lon, lat, speed,
           heading, in_out_of_harbor, hid = harborid)

}
