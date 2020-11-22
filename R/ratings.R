#' Notes presse des films sortis entre 2000 et 2019
#'
#' Base de données constituée à partir des notes presse d'Allociné
#' pour tous les films parus entre 2000 et 2019.
#'
#' @format Une base de données avec 174 222 notes et 9 variables :
#' \describe{
#'   \item{paper}{Nom de la publication (journal, magazine, site internet, ...)}
#'   \item{rating}{Note sur 5 que la publication a accordé au film}
#'   \item{title}{Titre du film}
#'   \item{date}{Date de sortie française en salles}
#'   \item{duration}{Durée du film en minutes}
#'   \item{genre}{Genre du film}
#'   \item{nationality}{Nationalité du film}
#'   \item{direction}{Réalisateur·ice}
#'   \item{actors}{Acteur·ices}
#' }
#' @source \url{https://allocine.fr}
"ratings"
