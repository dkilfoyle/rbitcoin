source("dword.R")

S0 = function(x) { x$ROTR(2)  ^ x$ROTR(13) ^ x$ROTR(22) }
S1 = function(x) { x$ROTR(6)  ^ x$ROTR(11) ^ x$ROTR(25) }
s0 = function(x) { x$ROTR(7)  ^ x$ROTR(18) ^ x$SHFTR(3) }
s1 = function(x) { x$ROTR(17) ^ x$ROTR(19) ^ x$SHFTR(10) }
Ch   = function(x, y, z) { (x & y) ^ (!x & z) }
Maj  = function(x, y, z) { (x & y) ^ (x & z) ^ (y & z) } 

sha256 = function(msg) {
  K = lapply(c(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2), function(x) DWORD$new(x))
  
  H = lapply(c(0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19), function(x) DWORD$new(x))
  
  # Package message into 512-bit blocks (array of 16 32-bit unsigned ints
  msg_utf8 = sapply(msg, function(x) utf8ToInt(x)) # convert message into array of utf8 bytes
  msg_utf8 = append(msg_utf8, 128) # the byte 128 = 10000000 = message terminator
  l32  = length(msg_utf8) / 4 + 2 # l32 is length in 32 bit word = 4 bytes/word + 2 length words
  N = ceiling(l32/16) # number of 16-word32 = 512 bit blocks
  M = vector(mode="list", length=N*16)
  
  # convert the bytesream to 32bit words = 4 bytes in big endian
  # DWORD internally stores as bit array because R cannot natively handle unsigned 32 bit integers
  mi = 1
  for (i in 1:N) {
    for (j in 1:16) {
      offset = (i-1)*64 + (j-1)*4 + 1
      if ((offset+3) > length(msg_utf8))
        M[[mi]] = DWORD$new(rep(0,32))
      else 
        M[[mi]] = DWORD$new(msg_utf8[offset:(offset+3)])
      mi = mi + 1
    }
  }
  M[[mi-2]] = DWORD$new(floor(((length(msg_utf8)-1)*8) / 2^32))
  M[[mi-1]] = DWORD$new(((length(msg_utf8)-1)*8) %% 2^32)
  
  dim(M) = c(N,16)
  
  for (i in 1:N) {
    # initialise registers to H
    a = H[[1]]$clone()
    b = H[[2]]$clone()
    cc = H[[3]]$clone()
    d = H[[4]]$clone()
    e = H[[5]]$clone()
    f = H[[6]]$clone()
    g = H[[7]]$clone()
    h = H[[8]]$clone()
    
    # prepare message schedule W
    W = vector(mode="list", length=64)
    for (t in 1:16) W[[t]] = M[[i,t]]
    for (t in 17:64) {
      W[[t]] = DWORD$new((s1(W[[t-2]]) + W[[t-7]] + s0(W[[t-15]]) + W[[t-16]]) %% 2^32)
    }
    
    # print out the message in DWORDs
    # sapply(W, function(x) print(x$getNum()))
    
    for (t in 1:64) {
      T1 = h + S1(e) + Ch(e, f, g) + K[[t]] + W[[t]]
      T2 = S0(a) + Maj(a, b, cc)
      # browser()
      h = g$clone()
      g = f$clone()
      f = e$clone()
      e$setNum((d + T1) %% 2^32)
      d = cc$clone()
      cc = b$clone()
      b = a$clone()
      a$setNum((T1 + T2) %% 2^32)
      # browser()
    }
    
    #4 - compute the new intermediate hash value
    H[[1]]$setNum((H[[1]]+a) %% 2^32)
    H[[2]]$setNum((H[[2]]+b) %% 2^32)
    H[[3]]$setNum((H[[3]]+cc) %% 2^32)
    H[[4]]$setNum((H[[4]]+d) %% 2^32)
    H[[5]]$setNum((H[[5]]+e) %% 2^32)
    H[[6]]$setNum((H[[6]]+f) %% 2^32)
    H[[7]]$setNum((H[[7]]+g) %% 2^32)
    H[[8]]$setNum((H[[8]]+h) %% 2^32)
    
    sapply(H, function(x) print(x$getNum()))
  }
  
  return(H)
}

HtoString = function(H) {
  paste0(sapply(H, function(x) x$getHex()), collapse="")
}

x=sha256("abc")

