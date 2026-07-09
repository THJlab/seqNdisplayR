# Color manipulation functions

#' Hex2Hsl
#'
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA (based on https://bitbucket.org/mattgove/color-theory/src/master/mg_colors.py)
#'
#' @param c 
#'
#' @return placeholder
#' 
#' @importFrom grDevices col2rgb rgb
#'
#' @examples
#' Hex2Hsl('black')
#' Hex2Hsl('blue')
#' Hex2Hsl('#FFA550')
#' 
Hex2Hsl = function(c){
  c_rgb_vector = as.vector(grDevices::col2rgb(c))/255
  r = c_rgb_vector[1]
  g = c_rgb_vector[2]
  b = c_rgb_vector[3]
  c_max = max(c_rgb_vector)
  c_min = min(c_rgb_vector)
  delta = c_max - c_min
  lightness = (c_max + c_min) / 2
  if (delta == 0){
    hue = 0
    saturation = 0
  }else{
    if (lightness < 0.5){
      saturation = delta / (c_max + c_min)
    }else{
      saturation = delta / (2 - c_max - c_min)
    } 
    delta_r = (((c_max - r)/6) + (delta/2)) / delta
    delta_g = (((c_max - g)/6) + (delta/2)) / delta
    delta_b = (((c_max - b)/6) + (delta/2)) / delta
    if (r == c_max){
      hue = delta_b - delta_g
    }else if (g == c_max){
      hue = (1/3) + delta_r - delta_b
    }else if (b == c_max){
      hue = (2/3) + delta_g - delta_r
    }
  }
  if (hue < 0){
    hue = hue + 1
  }else if (hue > 1){
    hue = hue - 1
  }
  hue = hue*360
  saturation = saturation*100
  lightness = lightness*100
  return(c(hue, saturation, lightness))
}


#' Adjust Color Phi
#'
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA (based on https://bitbucket.org/mattgove/color-theory/src/master/mg_colors.py)
#'
#' @param c_hsl 
#' @param phi 
#'
#' @return placeholder
#'
#' @examples
#' c_hsl = Hex2Hsl('#FFA550')
#' c_hsl_180 = AdjustColorPhi(c_hsl)
#' 
AdjustColorPhi = function(c_hsl, phi=180){
  hue = c_hsl[1] + phi
  if (hue >= 360){
    hue = hue - 360
  }else if (hue < 0){
    hue = hue + 360
  }
  adjusted_c_hsl = c(hue, c_hsl[2:3])
  return(adjusted_c_hsl)
}


#' Hsl2Hex
#'
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA (based on https://www.rapidtables.com/convert/color/hsl-to-rgb.html)
#'
#' @param c_hsl 
#'
#' @return placeholder
#' 
#' @importFrom grDevices col2rgb rgb
#'
#' @examples
#' c_hex = '#FFA550'
#' c_hsl = Hex2Hsl(c_hex)
#' c_hsl_180 = AdjustColorPhi(c_hsl)
#' c_hex_180 = Hsl2Hex(c_hsl_180)
#' 
Hsl2Hex = function(c_hsl){
  h = c_hsl[1]
  s = c_hsl[2]/100
  l = c_hsl[3]/100
  c = (1 - abs(2*l - 1)) * s
  x = c * (1 - abs( (h / 60) %% 2 - 1 ) )
  m = l - c/2
  if (h >= 0 & h < 60){
    rgb_prime = c(c, x, 0)
  }else if (h >= 60 & h < 120){
    rgb_prime = c(x, c, 0)
  }else if (h >= 120 & h < 180){
    rgb_prime = c(0, c, x)
  }else if (h >= 180 & h < 240){
    rgb_prime = c(0, x, c)
  }else if (h >= 240 & h < 300){
    rgb_prime = c(x, 0, c)
  }else if (h >= 300 & h < 360){
    rgb_prime = c(c, 0, x)
  }
  RGB = c(rgb_prime[1]+m, rgb_prime[2]+m, rgb_prime[3]+m)
  return(grDevices::rgb(RGB[1], RGB[2], RGB[3]))
}


#' Convert Color
#'
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA 
#'
#' @param c 
#' @param phi 
#'
#' @return placeholder
#'
#' @examples
#' c_hex = '#FFA550'
#' c_hex_180 = ConvertColor(c_hex)
#' 
ConvertColor = function(c, phi=180){
  c_hsl = Hex2Hsl(c)
  c_hsl_conv = AdjustColorPhi(c_hsl, phi)
  c_conv = Hsl2Hex(c_hsl_conv)
  return(c_conv)
}


#' Change Color Lightness
#'
#' @description Internal function: 
#' if the lightness value is <0.5 the color is considered dark
#' if the lightness value is >=0.5 the color is considered light
#'
#' @keywords internal
#'  
#' @author SLA 
#'
#' @param c 
#'
#' @return placeholder
#'
#' @examples
#' c_hex = '#FFA550'
#' c_new = ChangeColorLightness(c_hex, 0.25)
#' 
ChangeColorLightness = function(hex_color, factor) {
  hsl_values = Hex2Hsl(hex_color)
  lightness = hsl_values[3] / 100
  saturation = hsl_values[2] / 100
  hue = hsl_values[1]
  new_saturation = saturation
  new_hue = hue # + 180
  if (lightness <= 0.5){ # dark
    new_lightness = pmax(0, pmin(lightness + factor, 1)) 
    if (saturation < 0.5){ 
      new_saturation = pmax(0, pmin(saturation + 0.5, 1))
      new_lightness = pmax(0, pmin(lightness + 2*factor, 1)) 
      new_hue = hue + 180
    }
  }else{ # light
    new_lightness = pmax(0, pmin(lightness - factor, 1))  
    if (saturation > 0.5){ 
      new_saturation = pmax(0, pmin(saturation - 0.5, 1))
      new_lightness = pmax(0, pmin(lightness - 2*factor, 1)) 
      new_hue = hue + 180
    }
  }
  hsl_values[3] = new_lightness * 100
  hsl_values[2] = new_saturation * 100
  if (new_hue >= 360){
    new_hue = new_hue - 360
  }else if (hue < 0){
    new_hue = new_hue + 360
  }
  hsl_values[1] = new_hue 
  new_hex_color = Hsl2Hex(hsl_values)
  return(new_hex_color)
}


