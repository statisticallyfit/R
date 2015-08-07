#install.packages("extrafont")

library(extrafont)
#font_import()

fonts()

pdf("plot_cm.pdf", family="CM Roman", width=5.5, height=5)

curve(dnorm, from=-3, to=3, main="Normal Distribution")
text(x=0, y=0.1, cex=1.5, expression(italic(y == frac(1, sqrt(2 * pi)) *
                                              e ^ {-frac(x^2, 2)} )))

dev.off()
embed_fonts("plot_cm.pdf", outfile="plot_cm_embed.pdf")
