library(hexSticker)
library(showtext)

# Colors: https://coolors.co/62959c-011c27-ffa69e 

font_add_google("Fira Sans")

showtext_auto()

make_logo <- function(img, dark = FALSE) {
    if (dark) {
        sticker(
            img,
            s_x = 1,
            s_y = .715,
            package = "FAS1002",
            p_size = 31,
            p_family = "Fira Sans",
            p_color = "#FFA59E",
            p_fontface = "bold",
            p_x = 1,
            p_y = 1.3,
            h_size = 1.75,
            h_fill = "#011C27",
            h_color = "#FFA59E",
            filename = paste0("_logo/FAS1002_logo_dark_",
                              Sys.time(),
                              ".png"),
            # url = "FAS1002.github.io",
            # u_color = "#62959C",
            # u_family = "Fira Sans",
            # u_size = 4,
            # u_y = .07
        )
    } else {
        
    sticker(
        img,
        s_x = 1,
        s_y = .715,
        package = "FAS1002",
        p_size = 31,
        p_family = "Fira Sans",
        p_color = "#011C27",
        p_fontface = "bold",
        p_x = 1,
        p_y = 1.3,
        h_size = 1.75,
        h_fill = "#FFA59E",
        h_color = "#011C27",
        filename = paste0("_logo/FAS1002_logo_light_", Sys.time(), ".png"),
        # url = "FAS1002.github.io",
        # u_color = "#62959C",
        # u_family = "Fira Sans",
        # u_size = 4,
        # u_y = .07
    )}
}

make_logo('_logo/coding_dark.svg', dark = TRUE)
