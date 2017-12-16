library(R6)

Bits = R6Class("Bits",
  public = list(
    bits = NULL,
    initialize = function(x) {
      if (missing(x)) bits=c()
      else if (is.vector(x) & max(x) < 2) self$bits = bits
      else stop("Ivalid argument to Bits$initialize")
    },
    print = function(...) {
      cat("Bits: ", self$bits, "\n", sep="")
      invisible(self)
    },
    append = function(x) {
      if (is.vector(x) & max(x) < 2)
        self$bits = append(self$bits, x)
      else if (is.numeric(x))
        self$bits = append(self$bits, num2bin(x))
      return(self)
    },
    ROTR = function(n) {
      # rotate x's bits n to the right, circle the least sig bits to most sig bits, x=32 bit word
      len = length(self$bits)
      DWORD$new(c(self$bits[(len-n+1):len], self$bits[1:(len-n)]))
      # return (self$SHFTR(n) | self$SHFTL(32-n))
    },
    SHFTR = function(n) {
      len = length(self$bits)
      DWORD$new(c(rep(0,n), self$bits[1:(len-n)]))
    },
    SHFTL = function(n) {
      len = length(self$bits)
      DWORD$new(c(self$bits[(n+1):len], rep(0,n)))
    }
  ))

DWORD = R6Class("DWORD",
  inherit = Bits,
  public = list(
    signed = F,
    initialize = function(x, signed=F) {
      if (is.vector(x) & (max(x) < 2) & (length(x)==32)) self$bits = as.integer(x)
      else if (is.vector(x) & (length(x)==4)) self$bits = as.vector(sapply(x, num2bin, 8, F))
      else if (is.numeric(x)) self$setNum(x)
      else if (is.character(x) & (substr(x,1,2) == "0x")) self$bits = hex2bin(x)
      else stop("Invalid argument to DWORD$initialize")
      self$signed = signed
    },
    print = function() {
      super$print()
      cat("Num: ", self$getNum(), "\n", sep="")
      cat("Hex: ", self$getHex(), "\n", sep="")
      invisible(self)
    },
    setNum = function(x) self$bits = num2bin(x, 32, self$signed),
    getNum = function(signed) bin2num(self$bits, ifelse(missing(signed), self$signed, signed)),
    getHex = function() bin2hex(self$bits, self$signed)
  ))

`^.DWORD` = function(o1, o2) DWORD$new(xor(o1$bits, o2$bits))
`&.DWORD` = function(o1, o2) DWORD$new(o1$bits & o2$bits)
`!.DWORD` = function(o1)     DWORD$new(!(o1$bits))
`|.DWORD` = function(o1, o2) DWORD$new(o1$bits | o2$bits)
`+.DWORD` = function(o1, o2) {
  if (inherits(o1, "DWORD")) f1 = o1$getNum(signed=T)
  if (inherits(o1, "numeric")) f1 = o1
  if (is.null(f1)) stop("+.DWORD invalid arguments")
  if (inherits(o2, "DWORD")) f2 = o2$getNum(signed=T)
  if (inherits(o2, "numeric")) f2 = o2
  if (is.null(f2)) stop("+.DWORD invalid arguments")
  return (f1+f2)
}

num2bin = function(x, bitsize, signed=F) {
  
  if (!missing(bitsize))
    if (bitsize == 32)
      if (signed == T) {
        if (abs(x) > 2147483647) 
          stop("num2bin: x exceeds maximum value for signed 32 bit int")
      } else {
        if (x > 4294967295)
          stop("num2bin: x exceeds maximum value for unsigned 32 bit int")
      }
    else if (bitsize == 16)
      if (signed == T) {
        if (abs(x) > 32767) 
          stop("num2bin: x exceeds maximum value for signed 16 bit int")
      } else {
        if (x > 65535)
          stop("num2bin: x exceeds maximum value for unsigned 16 bit int")
      }
    if (bitsize == 8)
      if (signed == T) {
        if (abs(x) > 127) 
          stop("num2bin: x exceeds maximum value for signed 8 bit int")
      } else {
        if (x > 255)
          stop("num2bin: x exceeds maximum value for unsigned 8 bit int")
      }
  
  bits =c()
  x = floor(x)
  while (x > 0) {
    bits = c(bits, x %% 2) # add remainder when divided by 2
    x = floor(x/2)
  }
  # if (signed) bits=c(bits, 1) # bits will be reversed
  if (!missing(bitsize))
    return(c(rep(0, bitsize-length(bits)), rev(bits)))
  else
    return(rev(bits))
}

bin2num = function(bits, signed=F) {
  signm = 1
  if (signed & (bits[1]==1)) {
    # find the position of the least significant 1 (ie from the right)
    leastsig1pos = tail(which(bits==1), n=1)
    # complement everything before the least sig 1
    bits = c(!bits[1:(leastsig1pos-1)], bits[leastsig1pos:length(bits)])
    signm = -1
  }
  return(signm*sum(bits * 2^((length(bits):1)-1)))
}

hex2bin = function(hex) {
  i1 = strtoi(paste0("0x", substr(hex, 3, 4)))
  i2 = strtoi(paste0("0x", substr(hex, 5, 6)))
  i3 = strtoi(paste0("0x", substr(hex, 7, 8)))
  i4 = strtoi(paste0("0x", substr(hex, 9, 10)))
  return (c(int2bin(i1,8), int2bin(i2,8), int2bin(i3,8), int2bin(i4,8)))
}

bin2hex = function(bits, signed=F) {
  hexdigits = c(as.character(0:9), letters[1:7])
  paste0("0x", paste(sapply(seq(1,32,4), function(x) 
    hexdigits[sum(bits[x:(x+3)] * c(8,4,2,1))+1]),collapse=""))
}

# getWord32 = function(bits, n) {
#   bits[((n-1)*32+1):(n*32)]
# }
# 
# ROTR = function(bits, n) {
#   # rotate x's bits n to the right, circle the least sig bits to most sig bits, x=32 bit word
#   c(bits[(32-n+1):32], bits[1:(32-n)])
# }
# 
# SHFTR = function(bits, n) {
#   length(bits) = length(bits) - n
#   c(rep(0,n), bits)
# }
# 
# num2hex = function(number, prefix="0x") {
#   # convert numbers larger than maxint to 8 hex digits
#   # if (number > 4294967295) stop("num2hex: number is greater than 32bit unsigned int max")
#   hex =""
#   for (i in 1:8) {
#     hex = paste0(as.hexmode(as.integer(number %% 16)), hex)
#     number = number / 16
#   }
#   return(paste0(prefix,hex))
# }

