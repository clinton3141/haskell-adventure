import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum[w*b | (w,b) <- zip weights bits]
              where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8:: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chopn :: Int -> [Bit] -> [[Bit]]
chopn _ [] = []
chopn n bits = take n bits : chopn n (drop n bits)

chop8 :: [Bit] -> [[Bit]]
chop8 = chopn 8

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

parityCheck :: [Bit] -> [Bit]
parityCheck bs  | (even . sum) bs = bs
                | otherwise = error "Parity check failed"


parityBit :: [Bit] -> Bit
parityBit = (`mod` 2) . sum

encode' :: String -> [Bit]
encode' = concat . map ((\bs -> parityBit bs : bs) . (make8 . int2bin . ord))

decode' :: [Bit] -> String
decode' = map (chr . bin2int . drop 1 . parityCheck) . chopn 9


transmit' :: ([Bit] -> [Bit]) -> String -> String
transmit' c = decode' . c . encode'

faultyChannel = tail

goodTransmitter = transmit' channel
badTransmitter = transmit' faultyChannel