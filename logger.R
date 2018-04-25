library(futile.logger)

fmt = "~t   ~n|~f   ~l   ~m"
layout <- layout.format(fmt)
flog.layout(layout)
flog.threshold(DEBUG)