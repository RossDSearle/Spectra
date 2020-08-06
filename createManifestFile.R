
library(shinyMobile)


create_manifest(
  path = 'c:/temp',
  name = "Spectra Cloud",
  shortName = "Spectra Cloud",
  description = "A Demo for the Spectra Cloud project",
  lang = "en-US",
  startUrl = "https://shiny.esoil.io/Spectra/",
  display = "standalone",
  background_color = "#3367D6",
  theme_color = "#3367D6",
  icon = data.frame(
    src = "icons/Spectra.png",
    sizes = "32x32", 10,
    types = "image/png"
  )
)
