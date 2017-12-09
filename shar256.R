

padMessage = function(m) {
  # return the message in binary, 1, 0, length of m in bits as 64bits
  m_utf8 = sapply(m, function(x) utf8ToInt(x)) # convert message into array of utf8 bytes
  m_utf8 = append(m_utf8, 128) # the bytes 128 = 10000000 = message terminator
  
  
  
  
  m_bits = as.vector(sapply(m_utf8, function(x) int2bin(x, 8))) # convert 8 bit bytes into 8 bits
  m_bits = c(m_bits, 1) # append a 1 bit
  m_bits = c(m_bits, rep(0, 448-length(m_bits))) # pad with 0 out to pos 448
  m_bits = c(m_bits, rep(0,32), int2bin(nchar(m)*8,32)) # num bits in m as 64bits
  return(m_bits)
}



`Σ0` = function(x) { return (ROTR(x, 2) ^ ROTR(x, 13) ^ ROTR(x, 22)) }
`Σ1` = function(x) { return (ROTR(x, 6) ^ ROTR(x, 11) ^ ROTR(x, 25)) }
`σ0` = function(x) { return (ROTR(x, 7) ^ ROTR(x, 18) ^ SHFTR(x, 3)) }
`σ1` = function(x) { return (ROTR(x, 1) ^ ROTR(x, 19) ^ SHFTR(x, 10)) }

Ch = function(x, y, z) { return ((x & y) ^ (!x & z)) }
Maj = function(x, y, z) { return ((x & y) ^ (x & z) ^ (y & z)) } 


sha256 = function(msg) {
  # Package message into 512-bit blocks (array of 16 32-bit unsigned ints
  msg_utf8 = sapply(msg, function(x) utf8ToInt(x)) # convert message into array of utf8 bytes
  msg_utf8 = append(msg_utf8, 128) # the byte 128 = 10000000 = message terminator
  l32  = length(msg_utf8) / 4 + 2 # l32 is length in 32 bit word = 4 bytes/word + 2 length words
  N = ceiling(l32/16) # number of 16-word32 = 512 bit blocks
  M = vector(mode="list", length=N*16)
  
  mi = 1
  for (i in 1:N) {
    for (j in 1:16) {
      offset = (i-1)*64 + (j-1)*4 + 1
      print(j)
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
  
  return(M)
  
  H = c(0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19)
  
  for (i in 1:N) {

    # initialise registers to H
    a = H[1]
    b = H[2]
    c = H[3]
    d = H[4]
    e = H[5]
    f = H[6]
    g = H[7]
    h = H[8]
    
    # convert the bitstream to 32bit words = 4*8 bit chars in big endian
    M = t(sapply(1:64, function(t) getWord32(m,t)))
    
    # prepare message schedule W
    for (t in 1:16) W[t] = M[t]
    for (t in 17:64) W[t] = σ1(W[t-2]) + W[t-7] + σ0(W[t-15]) + W[t-16]
  
    for (j in 1:64) {
      b = a
      c = b
      
      che = Che(e, f, g)
      maj = Maj(a, b, c)
      sig0 = Sig0(a)
      sig1a= hash[1]
    }
  }
}

m=padMessage("abc")
x=DWORD$new(getWord32(m,1))
y=DWORD$new(getWord32(m,2))
z=DWORD$new(getWord32(m,3))
Σ0(x$bits)

x=DWORD$new("0x6a09e667")
x$getHex()

x=sha256("abc")

