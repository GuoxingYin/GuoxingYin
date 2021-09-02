#当图片不能使用配色包，需要调出颜色pal,调整程序内的颜色
library(ggsci)
library(scales)
pal <- pal_npg("nrc", alpha=1)(12) #nrc是Palette Types，alpha用于调节透明度
show_col(pal)
#当图片可以直接使用配色包时
#p = $p
#p2 = p + scale_color_npg()+ scale_fill_npg()
