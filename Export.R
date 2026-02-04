GA_ID <- "G-WXTZXFP1V2"

ga_head <- paste0(
  '<script async src="https://www.googletagmanager.com/gtag/js?id=', GA_ID, '"></script>',
  '<script>',
  'window.dataLayer = window.dataLayer || [];',
  'function gtag(){dataLayer.push(arguments);}',
  "gtag('js', new Date());",
  "gtag('config', '", GA_ID, "');",
  '</script>'
)

unlink("docs", recursive = TRUE, force = TRUE)

shinylive::export(
  appdir = ".",
  destdir = "docs",
  template_params = list(
    title = "Intuition Lab - Sampling & Spread",
    include_in_head = ga_head
  )
)

httpuv::runStaticServer("docs", port = 8000)
