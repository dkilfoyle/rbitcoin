library(R6)

Bits = R6Class("Bits",
  public = list(
    bits = NULL,
    initialize = function(x) {
      if (missing(x)) bits=c()
      else if (is.vector(x) & max(x) < 2) self$bits = bits
      else stop("Ivalid argument to Bits$initialize")
    },
    append = function(x) {
      if (is.vector(x) & max(x) < 2)
        self$bits = append(self$bits, x)
      else if (is.numeric(x))
        self$bits = append(self$bits, num2bin(x))
      return(self)
    }
  ))

DWORD = R6Class("DWORD",
  inherit = Bits,
  public = list(
    initialize = function(x) {
      if (is.vector(x) & (max(x) < 2) & (length(x)==32)) self$bits = x
      else if (is.vector(x) & (length(x)==4)) self$bits = sapply(x, num2bin, 8)
      else if (is.numeric(x)) self$bits = num2bin(x, 32)
      else if (is.character(x) & (substr(x,1,2) == "0x")) self$bits = hex2bin(x)
      else stop("Invalid argument to DWORD$initialize")
    },
    
    getNum = function() sum(self$bits * 2^((32:1)-1)),
    getHex = function() {
      hexdigits = c(as.character(0:9), letters[1:7])
      paste0("0x", paste(sapply(seq(1,32,4), function(x) hexdigits[sum(self$bits[x:(x+3)] * c(8,4,2,1))+1]),collapse=""))
    }
  ))

`^.DWORD` = function(o1, o2) DWORD$new(as.integer(xor(o1$bits, o2$bits)))
`&.DWORD` = function(o1, o2) DWORD$new(as.integer(o1$bits & o2$bits))
`!.DWORD` = function(o1) DWORD$new(as.integer(!(o1$bits)))


num2hex = function(number, prefix="0x") {
  # convert numbers larger than maxint to 8 hex digits
  # if (number > 4294967295) stop("num2hex: number is greater than 32bit unsigned int max")
  hex =""
  for (i in 1:8) {
    hex = paste0(as.hexmode(as.integer(number %% 16)), hex)
    number = number / 16
  }
  return(paste0(prefix,hex))
}


num2bin = function(x, bitsize) {
  bits =c()
  x = floor(x)
  while (x > 0) {
    bits = c(bits, x %% 2) # add remainder when divided by 2
    x = floor(x/2)
  }
  if (!missing(bitsize))
    return(c(rep(0, bitsize-length(bits)), rev(bits)))
  else
    return(rev(bits))
}

bin2num = function(x) sum(x * 2^((32:1)-1))

int2bin = function(number, noBits) {
  if (number > .Machine$integer.max) stop("Number is greater than integer max")
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits) | (noBits==32)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}

bin2int = function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")[1]
}

hex2bin = function(hex) {
  i1 = strtoi(paste0("0x", substr(hex, 3, 4)))
  i2 = strtoi(paste0("0x", substr(hex, 5, 6)))
  i3 = strtoi(paste0("0x", substr(hex, 7, 8)))
  i4 = strtoi(paste0("0x", substr(hex, 9, 10)))
  return (c(int2bin(i1,8), int2bin(i2,8), int2bin(i3,8), int2bin(i4,8)))
}

getWord32 = function(bits, n) {
  bits[((n-1)*32+1):(n*32)]
}

ROTR = function(bits, n) {
  # rotate x's bits n to the right, circle the least sig bits to most sig bits, x=32 bit word
  c(bits[(32-n+1):32], bits[1:(32-n)])
}

SHFTR = function(bits, n) {
  length(bits) = length(bits) - n
  c(rep(0,n), bits)
}

