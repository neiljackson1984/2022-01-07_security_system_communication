from typing import Dict, Iterable, List, Optional, Sequence
import typing
import serial;
import datetime;
import pathlib;
import itertools;
import sys;

pathOfLogDirectory = pathlib.Path(__file__).parent.joinpath("logs")
pathOfLogDirectory.mkdir(exist_ok=True)
timeOfLaunch = datetime.datetime.now()
pathOfLogFile = pathOfLogDirectory.joinpath("log_{:%Y%m%d-%H%M}.txt".format(timeOfLaunch))


pathOfChangeLogFile = pathOfLogDirectory.joinpath("change_log_{:%Y%m%d-%H%M}.txt".format(timeOfLaunch))



#region printableRADXCodePage
if True: # (region) printableRADXCodePage
    printableRADXCodePage : Dict[int, Optional[str]] = {
        # The basic scheme seems to be that we take the low six bits, find the
        # number in the range (0x20, .., 0x5F) that has those low six bits, and
        # then take the character that number points to in the ASCII table. This
        # is the "SixBit ASCII" encoding mentioned in the Wikipedia article
        # https://en.wikipedia.org/wiki/Six-bit_character_code we completely
        # ignore the highest two-bits.
        #
        # In the date-time readout string, where we would expect to see a colon,
        # between the hours and the minutes, we actually observe that the low
        # six bits are 0x29, which points to a right-parenthesis in the SixBit
        # ASCII lookup table. I am interpreting this discrepancy not as a
        # departure from the SixBit ASCII encoding, but rather as a conscious
        # choice by the designers to use a right-paranethesis in place of a
        # colon due to the way the graphical appearance of the characters on the
        # 14-segment character displays in the RADX keypad; either the
        # 14-segment display simply is incapable of displaying a colon, or the
        # 14-segment's rendering of a right parenthesis is somehow more
        # intelligible as an hour-minute delimeter than the 14-segment's
        # rendering of a colon.
        #
        # The display on the RADX keypad, which I have been calling a 14-segment
        # display, actually seems to have a total of 16
        # individually-controllable segments per character position; in addition
        # to the usual 14 segments that comprise the main character, there is,
        # next to the bottom right corner of each character, as if a subscript,
        # a "semicolon" arrangement of a "period" on top of a "comma".
        # Possibly, the upper two bits of each byte control the "comma" and
        # "period" punctuation that follows the character that is encoded by the
        # lower 6 bits of the byte.
        
        #   ___________________________________________________
        #   |codepoint                                        |
        #   |     ____________________________________________|
        #   |     |printable string                           |
        #   |     |according to                               |
        #   |     |RADX encoding                              |
        #   |     |       ____________________________________|
        #   |     |       |ascii printable                    |
        #   |     |       |string (if different from RADX)    |
        #   |     |       |      _____________________________|
        #   |     |       |      |speculative (delete the     |
        #   |     |       |      |asterisk upon observing     |
        #   |     |       |      |an example of this encoding |
        #   |     |       |      |in the real-world byte      |
        #   |     |       |      |stream sniffed from/to      |
        #   |     |       |      |the RADX keypad)            |
        #   |     |       |      |                            |
            0x00: "@" , # None , *                            |
            0x01: "A" , # None ,                              |
            0x02: "B" , # None ,                              |
            0x03: "C" , # None ,                              |
            0x04: "D" , # None , *                            |
            0x05: "E" , # None ,                              |
            0x06: "F" , # None ,                              |
            0x07: "G" , # None , *                            |
            0x08: "H" , # None , *                            |
            0x09: "I" , # None ,                              |
            0x0a: "J" , # None , *                            |
            0x0b: "K" , # None , *                            |
            0x0c: "L" , # None , *                            |
            0x0d: "M" , # None ,                              |
            0x0e: "N" , # None , *                            |
            0x0f: "O" , # None ,                              |
            0x10: "P" , # None , *                            |
            0x11: "Q" , # None , *                            |
            0x12: "R" , # None ,                              |
            0x13: "S" , # None , *                            |
            0x14: "T" , # None ,                              |
            0x15: "U" , # None ,                              |
            0x16: "V" , # None ,                              |
            0x17: "W" , # None , *                            |
            0x18: "X" , # None , *                            |
            0x19: "Y" , # None , *                            |
            0x1a: "Z" , # None , *                            |
            0x1b: "[" , # None , *                            |
            0x1c: "\\", # None , *                            |
            0x1d: "]" , # None , *                            |
            0x1e: "^" , # None , *                            |
            0x1f: "_" , # None , *                            |
            0x20: " " , #      , *                            |
            0x21: "!" , #      , *                            |
            0x22: "\"", #      , *                            |
            0x23: "#" , #      , *                            |
            0x24: "$" , #      , *                            |
            0x25: "%" , #      , *                            |
            0x26: "&" , #      , *                            |
            0x27: "'" , #      , *                            |
            0x28: "(" , #      , *                            |
            0x29: ")" , #      ,                              |
            0x2a: "*" , #      , *                            |
            0x2b: "+" , #      , *                            |
            0x2c: "," , #      , *                            |
            0x2d: "-" , #      , *                            |
            0x2e: "." , #      , *                            |
            0x2f: "/" , #      ,                              |
            0x30: "0" , #      ,                              |
            0x31: "1" , #      ,                              |
            0x32: "2" , #      , *                            |
            0x33: "3" , #      , *                            |
            0x34: "4" , #      , *                            |
            0x35: "5" , #      ,                              |
            0x36: "6" , #      , *                            |
            0x37: "7" , #      , *                            |
            0x38: "8" , #      ,                              |
            0x39: "9" , #      , *                            |
            0x3a: ":" , #      , *                            |
            0x3b: ";" , #      , *                            |
            0x3c: ">" , #      , *                            |
            0x3d: "=" , #      , *                            |
            0x3e: ">" , #      , *                            |
            0x3f: "?" , #      , *                            |
        #   0x40: "@" , # "@"  , *                            |
        #   0x41: "A" , # "A"  , *                            |
        #   0x42: "B" , # "B"  , *                            |
        #   0x43: "C" , # "C"  , *                            |
        #   0x44: "D" , # "D"  , *                            |
        #   0x45: "E" , #      ,                              |
        #   0x46: "F" , # "F"  , *                            |
        #   0x47: "G" , # "G"  , *                            |
        #   0x48: "H" , # "H"  , *                            |
        #   0x49: "I" , # "I"  , *                            |
        #   0x4a: "J" , # "J"  , *                            |
        #   0x4b: "K" , # "K"  , *                            |
        #   0x4c: "L" , #      ,                              |
        #   0x4d: "M" , #      ,                              |
        #   0x4e: "N" , # "N"  , *                            |
        #   0x4f: "O" , # "O"  , *                            |
        #   0x50: "P" , #      ,                              |
        #   0x51: "Q" , # "Q"  , *                            |
        #   0x52: "R" , # "R"  , *                            |
        #   0x53: "S" , #      ,                              |
        #   0x54: "T" , # "T"  , *                            |
        #   0x55: "U" , # "U"  , *                            |
        #   0x56: "V" , # "V"  , *                            |
        #   0x57: "W" , # "W"  , *                            |
        #   0x58: "X" , # "X"  , *                            |
        #   0x59: "Y" , # "Y"  , *                            |
        #   0x5a: "Z" , # "Z"  , *                            |
        #   0x5b: "[" , # "["  , *                            |
        #   0x5c: "\\", # "\\" , *                            |
        #   0x5d: "]" , # "]"  , *                            |
        #   0x5e: "^" , # "^"  , *                            |
        #   0x5f: "_" , # "_"  , *                            |
        #   0x60: " " , # "`"  ,                              |
        #   0x61: "a" , # "a"  , *                            |
        #   0x62: "b" , # "b"  , *                            |
        #   0x63: "c" , # "c"  , *                            |
        #   0x64: "d" , # "d"  , *                            |
        #   0x65: "e" , # "e"  , *                            |
        #   0x66: "f" , # "f"  , *                            |
        #   0x67: "g" , # "g"  , *                            |
        #   0x68: "h" , # "h"  , *                            |
        #   0x69: "i" , # "i"  , *                            |
        #   0x6a: "j" , # "j"  , *                            |
        #   0x6b: "k" , # "k"  , *                            |
        #   0x6c: "l" , # "l"  , *                            |
        #   0x6d: "m" , # "m"  , *                            |
        #   0x6e: "n" , # "n"  , *                            |
        #   0x6f: "o" , # "o"  , *                            |
        #   0x70: "p" , # "p"  , *                            |
        #   0x71: "1" , # "q"  ,                              |
        #   0x72: "r" , # "r"  , *                            |
        #   0x73: "3" , # "s"  ,                              |
        #   0x74: "t" , # "t"  , *                            |
        #   0x75: "u" , # "u"  , *                            |
        #   0x76: "v" , # "v"  , *                            |
        #   0x77: "w" , # "w"  , *                            |
        #   0x78: "x" , # "x"  , *                            |
        #   0x79: "y" , # "y"  , *                            |
        #   0x7a: "z" , # "z"  , *                            |
        #   0x7b: "{" , # "{"  , *                            |
        #   0x7c: "|" , # "|"  , *                            |
        #   0x7d: "}" , # "}"  , *                            |
        #   0x7e: "~" , # "~"  , *                            |
        #   0x7f: None, # None , *                            |
    }
#endregion

def bytesToPrintableRADXString(x : bytes) -> str:
    return "".join(
        map(
            lambda x: 
                printableRADXCodePage[x & 0b00111111], 
            x
        )
    )

def bytesToHighTwoBitReadableString(x : bytes) -> str:
    return " ".join(
        map(
            lambda x: 
                {
                    0b00000000: "--",
                    0b01000000: "-*",
                    0b10000000: "*-",
                    0b11000000: "**",
                }[x & 0b11000000],
            x
        )
    )

def log(x: str):
    # print(x,end='')
    logFile.write(x)


def updateStaticOutputLine(x: str, lineNumber: int = 1):

    # writes characters to stdout so as to erase an overwrite the current line of output (rather than writing a whole new line of output)
    print(
        "\N{escape}[" + str(lineNumber) + ";1H" 
        + x,
        end=''
    )
    sys.stdout.flush()

def clearScreen():
    print(
        "\N{escape}[2J",
        end=''
    )

def changeLog(x: str):
    changeLogFile.write(x)
    changeLogFile.flush()


def checksum(x: Iterable[int]) -> int:
    a : int = 0
    for i in range(0,len(x)):
        a = 0xff & (a + ( x[i] & 0x7f if i == 0 else x[i] )) 
    return 0xff - a

def isStartByte(x: int) -> bool:
    return x in (0x01, 0x64, 0x81)

def getExpectedLength(byte0: int, byte1: int) -> int:
    # return (byte1 + 3 if byte0 == 0x81 else byte1 + 2)
    return byte1 + 2
# a packet consists of:
# startByte  n  payload[0] payload[1] ... payload[n-2]  checksum

def codepointIsPrintable(codepoint : int) -> bool:
    # return 0x20 <= codepoint <= 0x7e # ASCII
    # return 0x00 <= codepoint <= 0x3f  #RADX
    return codepoint in printableRADXCodePage

def makePacket(address: int, payload: bytes) -> bytes:
    packetExceptForChecksum = bytes((address, len(payload) + 1, *payload))
    return bytes((*packetExceptForChecksum, checksum(packetExceptForChecksum)))


interClusterThresholdInterval = datetime.timedelta(milliseconds = 150)

class State:
    def __init__(self):
        self._length12AndStartsWithDigit   :bytes = bytes()
        self._otherLengthGreaterThanFour   :bytes = bytes()
        self._lengthLessThanOrEqualToFour  :bytes = bytes()

        self._LastValueOfLength12AndStartsWithDigit   :bytes = bytes()
        self._LastValueOfOtherLengthGreaterThanFour   :bytes = bytes()
        self._LastValueOfLengthLessThanOrEqualToFour  :bytes = bytes()
    
        self._timeOfLastChangeOfLength12AndStartsWithDigit  : datetime.datetime = datetime.datetime.now()
        self._timeOfLastChangeOfOtherLengthGreaterThanFour  : datetime.datetime = datetime.datetime.now()
        self._timeOfLastChangeOfLengthLessThanOrEqualToFour : datetime.datetime = datetime.datetime.now()
        self._timeOfLastChange                              : datetime.datetime = datetime.datetime.now()

        self._setCountOfLength12AndStartsWithDigit   : int = 0
        self._setCountOfOtherLengthGreaterThanFour   : int = 0
        self._setCountOfLengthLessThanOrEqualToFour  : int = 0
        self._setCount                               : int = 0

        
        self._changeCountOfLength12AndStartsWithDigit   : int = 0
        self._changeCountOfOtherLengthGreaterThanFour   : int = 0
        self._changeCountOfLengthLessThanOrEqualToFour  : int = 0
        self._changeCount                               : int = 0


    @property 
    def length12AndStartsWithDigit(self):
        return self._length12AndStartsWithDigit 
    @length12AndStartsWithDigit.setter
    def length12AndStartsWithDigit(self, newValue : Sequence[int]):
        self._setCount += 1
        self._setCountOfLength12AndStartsWithDigit += 1
        if bytes(newValue) != self.length12AndStartsWithDigit:
            self._pushCurrentToLast()
            self._length12AndStartsWithDigit = bytes(newValue)
            self._timeOfLastChangeOfLength12AndStartsWithDigit = datetime.datetime.now()
            self._timeOfLastChange = self._timeOfLastChangeOfLength12AndStartsWithDigit
            self._changeCountOfLength12AndStartsWithDigit += 1
            self._changeCount += 1
    

    @property 
    def otherLengthGreaterThanFour(self):
        return self._otherLengthGreaterThanFour 
    @otherLengthGreaterThanFour.setter
    def otherLengthGreaterThanFour(self, newValue : Sequence[int]):
        self._setCount += 1
        self._setCountOfOtherLengthGreaterThanFour += 1
        if bytes(newValue) != self._otherLengthGreaterThanFour:
            self._pushCurrentToLast()
            self._otherLengthGreaterThanFour = bytes(newValue)
            self._timeOfLastChangeOfOtherLengthGreaterThanFour = datetime.datetime.now()
            self._timeOfLastChange = self._timeOfLastChangeOfOtherLengthGreaterThanFour
            self._changeCountOfOtherLengthGreaterThanFour += 1
            self._changeCount += 1            
    
    @property 
    def lengthLessThanOrEqualToFour(self):
        return self._lengthLessThanOrEqualToFour 
    @lengthLessThanOrEqualToFour.setter
    def lengthLessThanOrEqualToFour(self, newValue : Sequence[int]):
        self._setCount += 1
        self._setCountOfLengthLessThanOrEqualToFour += 1
        if bytes(newValue) != self._lengthLessThanOrEqualToFour:
            self._pushCurrentToLast()
            self._lengthLessThanOrEqualToFour = bytes(newValue)
            self._timeOfLastChangeOfLengthLessThanOrEqualToFour = datetime.datetime.now()
            self._timeOfLastChange = self._timeOfLastChangeOfLengthLessThanOrEqualToFour
            self._changeCountOfLengthLessThanOrEqualToFour += 1
            self._changeCount += 1

    def _pushCurrentToLast(self):
        self._LastValueOfLength12AndStartsWithDigit   = self._length12AndStartsWithDigit   
        self._LastValueOfOtherLengthGreaterThanFour   = self._otherLengthGreaterThanFour   
        self._LastValueOfLengthLessThanOrEqualToFour  = self._lengthLessThanOrEqualToFour  


    def getReportString(self):
        # return self._otherLengthGreaterThanFour.hex(" ") + " | " + self._length12AndStartsWithDigit.hex(" ") + " | " + self._lengthLessThanOrEqualToFour.hex(" ")

        def byteWithDiffMark(byteOfCurrent, byteofLast):
            return  bytes((byteOfCurrent,)).hex() + (" " if byteOfCurrent == byteofLast else "*")

        return (
            "".join(
                map(
                    byteWithDiffMark,
                    self._otherLengthGreaterThanFour, 
                    self._LastValueOfOtherLengthGreaterThanFour
                )
            ) + "| "
            + "".join(
                map(
                    byteWithDiffMark,
                    self._length12AndStartsWithDigit, 
                    self._LastValueOfLength12AndStartsWithDigit
                )
            ) + "| "
            + "".join(
                map(
                    byteWithDiffMark,
                    self._lengthLessThanOrEqualToFour, 
                    self._LastValueOfLengthLessThanOrEqualToFour
                )
            ) + "| "
        )


# Our packet parsing algorithm is as follows:
# Starting with a start-of-cluster (We can't quite rely on a start byte to mark the start of a packet because the same values as the start bytes might appear as a checksum (or a payload byte, for all we know))
# we look for the first start byte (collecting any apparent garbage along the way for later analysis).
# (to get fancy, we could dynamically adjust the inter-cluster holdoff interval in the case of extremely long clusters (which indicate that our inter-cluster timing detection scheme is probably not working))
# The goal is to parse the incoming byte stream (along the timing-based guesses at cluster division) into a sequence of packets.
# we insist that a packet must lie entirely in one cluster, although a cluster can contain more than one packet.


# a "chunk" is a sequence of bytes received in a single reading of the input buffer
# a great many "chunks" will be empty because we are polling the buffer much faster than 
# data is actually arriving.

# a chunk of data received "At one time" on the serial port.  
# we hang onto chunks du7ring processing because the chunks contian the timing information.

class KeypadState:
    def __init__(self, address : int = 0x01, outputLineNumber : int = 1):
        self.address                    : int                = address
        self.displayReadout             : str                = ""
        self.bitMapReadout              : str                = ""
        self.lastReceivedCommand        : bytes              = bytes()
        self.lastReceivedPayload        : bytes              = bytes()

        self.lastSentCommand            : bytes              = bytes()
        self.lastNonEmptySentCommand    : bytes              = bytes()

        self.lastReceivedCommand            : bytes              = bytes()
        self.lastNonEmptyReceivedCommand    : bytes              = bytes()

        self.lastReceivedAudioDirective   : bytes                  = bytes()

        self.lastUpdateTime             : datetime.datetime  = datetime.datetime.now()
        self._fieldLengths              : Sequence[int]          = []
        self._reportString              : str                = ""

        self.outputLineNumber = outputLineNumber
    def getReportString(self) -> str:
        return self._reportString

    def update(self, addressAndDirection: int, payload: bytes(), chunks: Optional[Sequence['Chunk']] = None) -> None:
        if (addressAndDirection & 0x7f) == self.address :
            self.lastUpdateTime = (chunks[0].arrivalTime if chunks else datetime.datetime.now())
            direction = ("outOfKeypad" if addressAndDirection & 0x80 else "intoKeypad")
            if direction == "outOfKeypad":
                self.lastSentCommand = payload
                if payload: self.lastNonEmptySentCommand = payload
            else:
                # in this case, the direction was intoKeypad.
                self.lastReceivedPayload = payload
                
                # printablePayload, separator, commandPayload = payload.rpartition(b'\x03')

                # separate the payload into a printablePartOfPayload, an audioDirective and a command.
                # the received payload always takes the form
                # <printablePartOfPayload> <audioDirective> <command>
                # I have only ever seen the following audioCommands:
                # b'\x03' ( 0000 0011 ): 
                #   silent (the noraml condition)


                # b'\x0b' ( 0000 1011 ): 
                #   a two-note falling phrase: G natural, E flat (sounds when a sensor is tripped while the system is armed, before the alarm goes off. often accompanies the message "DISARM NOW")
                #   likely match from section 2.3 of the D1255 manual: "Entrance Warning"

                # b'\x07' ( 0000 0111 ): 
                #   single  E natural (sounds when you enter an invalid code on the keypad.)
                #   likely match from section 2.3 of the D1255 manual: "Invalid Key Buzz"

                # b'\x23' ( 0010 0011 ): 
                #   single  A natural (sounds during the "ARMING...EXIT NOW" countdown) 
                #   likely match from section 2.3 of the D1255 manual: "Exit Warning"

                # b'\x43' ( 0100 0011 ): 
                #   rapidly-pulsing G sharp. (this sounds continuously when an alarm is going off)
                #   likely match from section 2.3 of the D1255 manual: "Burglary Signal"

                # b'\x13' ( 0001 0011 ): 
                #   rapidly-pulsing E flat (sounds when a door opens while watch mode is enabled )
                #   likely match from section 2.3 of the D1255 manual: "xxxx"

                # b'\x83' ( 1000 0011 ): 
                #   b flat. Sounds when the "FIRE" alarm is going off.
                #   likely match from section 2.3 of the D1255 manual: "Fire Signal"

                # Is suspect that what I am interpreting as the "audio directive" might serve more purpose than just audio control, but who knows.
                # in addition to the externally-commanded audio that is caused by the audioCommands, the keypad also emits a beep (frequency: B natural)
                # whenever a key is pressed (and the outgoing buffer is not full) (likely match from section 2.3 of the D1255 manual: "Keypad Encoding Tone")
                # 
                # unmatched table entries from the list in section 2.3 of the D1255 manual: 
                # 1. "Watch Tone" (the manual describes this as being the same as the "Entrance Warning" signal, so the code might well be identical)
                # 2. "Trouble Buzzer" 

                audioDirective         = ( payload[:1]  if len(payload) < 4  else payload[-1:] )
                printablePartOfPayload = ( b''          if len(payload) < 4  else payload[:-1] )
                command                = ( payload[1:]  if len(payload) < 4  else b''          )
                # the above algorithm is probably not the same as was originally designed, but it 
                # seems to work empirically at least for the cases I have encountered.

                
                if printablePartOfPayload: self.displayReadout = bytesToPrintableRADXString(printablePartOfPayload) 

                self.lastReceivedCommand = command
                if command: self.lastNonEmptyReceivedCommand = command
                
                self.lastReceivedAudioDirective = audioDirective

                if len(payload) > 3 :
                    self.bitMapReadout = bytesToHighTwoBitReadableString(payload)
            
            fieldContentStrings = [
                f"{self.lastUpdateTime}",
                ("sent     " if direction == "outOfKeypad" else 
                 "received "
                ) + payload.hex(" "), 
                ("sent     " if direction == "outOfKeypad" else 
                 "received "
                ) + bytesToPrintableRADXString(payload), 
                self.lastReceivedCommand.hex(" "),
                self.lastNonEmptyReceivedCommand.hex(" "),
                self.lastSentCommand.hex(" "),
                self.lastNonEmptySentCommand.hex(" "),
                self.bitMapReadout,
                # bytesToPrintableRADXString(self.lastReceivedPayload),
                self.lastReceivedAudioDirective.hex(" "),
                self.displayReadout,
            ]


            self._fieldLengths = list(
                map(max, 
                    itertools.zip_longest(
                        self._fieldLengths, 
                        map(len, fieldContentStrings),
                        fillvalue=0
                    )
                )
            )

            paddedFields = map(
                lambda contentString, fieldLength: contentString.ljust(fieldLength),
                fieldContentStrings,
                self._fieldLengths
            )

            self._reportString = " | ".join(paddedFields)
            changeLog(self._reportString + "\n")
            updateStaticOutputLine(self._reportString, self.outputLineNumber)

class ProgrammerState(KeypadState):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        # self.nextResponsePayload = b'\x00\x0e'
        self.nextResponsePayload = b''
        self.nextResponseAddress = 0x09 #0x64 + 0x80
        self.lastResponse : Optional[bytes] = None
        
    
    def update(self, addressAndDirection: int, payload: bytes(), chunks: Optional[Sequence['Chunk']] = None):
        
        if (addressAndDirection & 0x7f) == self.address :
            if addressAndDirection & 0x80:
                pass
            else:
                response = None
                if payload == b'\x09':
                    # response = makePacket(self.address + 0x80, self.nextResponsePayload +  b'\x0e')
                    # response = makePacket(self.address + 0x80, b'\x02\x05\x06' +  b'\x0e')
                    response = makePacket(self.nextResponseAddress, self.nextResponsePayload )
                    if self.nextResponsePayload[0] == 0xff:
                        self.nextResponseAddress = (self.nextResponseAddress + 1) & 0xff
                    self.nextResponsePayload = bytes( (self.nextResponsePayload[0], (self.nextResponsePayload[1] + 1) & 0xff , *self.nextResponsePayload[2:]) )
                    # response = makePacket(0x64 + 0x80, b'\x06\x05\x04\x03\x02\x01\x0e')
                    # response = makePacket(0x64 + 0x80, b'\x0e')
                    # response = makePacket(0x64 + 0x80, b'\x09\x09\x0e')
                    # response = makePacket(0x65 + 0x80, b'\x01\x02\x05\x06\x08\x0e')

                    global serialPort
                    serialPort.write(response)
                    self.lastResponse = response

                    message = f"{datetime.datetime.now()}: " + "responding " + response.hex(" ")
                    updateStaticOutputLine(message,5)
                else:
                    message = f"{datetime.datetime.now()}: programmer {hex(self.address)} received an unusual payload: " + payload.hex(" ") + ". lastResponse: " + ("None" if self.lastResponse is None else self.lastResponse.hex(" "))
                    updateStaticOutputLine(message,6)
                    
                    
                    
            super().update(addressAndDirection, payload, chunks)        
            log(message + "\n")
       
        

class Chunk:
    lastChunkIndex : int = -1
    def __init__(self, byteArray : Iterable[int]):
        self.arrivalTime = datetime.datetime.now()
        self.byteArray = byteArray
        self.index = self.lastChunkIndex + 1
        # self.index = Chunk.lastChunkIndex + 1
        self.lastChunkIndex = self.index

if __name__ == "__main__":
    logFile = open(pathOfLogFile, 'a')
    changeLogFile = open(pathOfChangeLogFile, 'a')

    serialPort = serial.Serial(
        port='COM3', 
        baudrate=9600, 
        bytesize=serial.EIGHTBITS, 
        parity=serial.PARITY_NONE, 
        stopbits=serial.STOPBITS_ONE, 
        timeout=None, 
        xonxoff=False, rtscts=False, dsrdtr=False
    )
    serialPort.reset_input_buffer()


    # state :State = State()
    keypadState : KeypadState = KeypadState(address = 0x01, outputLineNumber=2)
    programmerState : ProgrammerState = ProgrammerState(address = 0x64, outputLineNumber=4)
    lastNonemptyChunk : Optional[Chunk] = None
    # iterationIndex = 0
    clusterCount = 0
    checksumFailureCount = 0
    packetCount = 0
    garbageCount = 0
    # iterationIndexOfLastReception = 0
    # numberOfReceptionsInThisCluster = 0
    # thisCluster : List[int] = []
    # timeOfLastReception = datetime.datetime.now()
    bufferOfNonemptyChunks : List[Chunk] = []


    def handleChunk(chunk : Chunk):
        global bufferOfNonemptyChunks
        global lastNonemptyChunk
        if chunk.byteArray:
            # in this case, we are dealing with a non-empty chunk
            if (lastNonemptyChunk is not None) and ( (chunk.arrivalTime - lastNonemptyChunk.arrivalTime) > interClusterThresholdInterval):
                handleCluster(bufferOfNonemptyChunks); bufferOfNonemptyChunks = []
            bufferOfNonemptyChunks.append(chunk)
            lastNonemptyChunk = chunk

    def handleCluster(chunks: Sequence[Chunk]):
        global clusterCount
        clusterCount += 1
        # byteIterator = iter
        garbageBytes : List[int] = []
        garbageChunks : List[Chunk] = [] # the set of chunks from which garbageBytes comes

        packetBytes : List[int] 
        packetChunks : List[Chunk] # the set of chunks from which packetBytes comes

        weAreReadingAPacket : bool = False
        targetLengthOfThisPacketIsKnown : bool 
        targetLengthOfThisPacket : int
        
        # we regard the cluster as consisting of an alternating sequence of garbage, packet, garbage, ..., packet, garbage
        # it is distinctly possible that garbageBytes might be part of a real packet, or a packet type that we do not know about, 
        # so we record garabage for later analysis.
        for thisByte, chunk in ( (thisByte, chunk) for chunk in chunks for thisByte in chunk.byteArray):  
            if (not weAreReadingAPacket) and isStartByte(thisByte):
                # switch to packet-reading mode, first handling any accumulated garbage.
                if garbageBytes: handleGarbage(garbageBytes, garbageChunks); garbageBytes = []; garbageChunks = []
                weAreReadingAPacket = True; packetBytes = []; packetChunks = []; targetLengthOfThisPacketIsKnown = False  
            
            if weAreReadingAPacket:
                packetBytes.append(thisByte); 
                if chunk not in packetChunks: packetChunks.append(chunk)
                if (not targetLengthOfThisPacketIsKnown) and len(packetBytes) == 2:
                    targetLengthOfThisPacket = getExpectedLength(*packetBytes[0:2])
                    targetLengthOfThisPacketIsKnown = True
                if targetLengthOfThisPacketIsKnown and len(packetBytes) == targetLengthOfThisPacket:
                    # switch to garbage-reading mode, first handling the accumulated packet.
                    handlePacket(packetBytes, packetChunks); packetBytes = []; packetChunks = []; 
                    weAreReadingAPacket = False; garbageBytes = []; garbageChunks = []
            else:
                garbageBytes.append(thisByte)
                if chunk not in garbageChunks: garbageChunks.append(chunk)

        # clean-up:
        if weAreReadingAPacket:
            # yikes: we have hit the end of the cluster without finsihing our packet
            # (i.e. without reaching the declared length of the packet, or possibly without even having a length declared at all (i.e. only having received the startByte) -- therefore
            # the packet is probably malformed, and we will treat it as garbage.
            if packetBytes: handleGarbage(packetBytes, packetChunks)
        else:
            if garbageBytes: handleGarbage(garbageBytes, garbageChunks)

    fieldLengths : List[int] = []

    def handlePacket(packetBytes : Sequence[int], packetChunks : Sequence[Chunk]):
        # declaredLength = packetBytes[1] + 2
        expectedLength = getExpectedLength(*packetBytes[0:2])
        assert expectedLength == len(packetBytes)
        # our packet-finding algorithm in handleCluster should only have called
        # handlePacket in cases where the declared length (i.e. the second byte of
        # the packet + 2) is equal to the length of packetBytes. it is a programming
        # error if this is not true here.
        global packetCount
        packetCount += 1


        declaredChecksum = packetBytes[-1]
        computedChecksum = checksum(packetBytes[0:-1])
        
        # we probably ought to send any packet with checksum failure to the garbage stream.

        address = packetBytes[0]
        payload = bytes(packetBytes[2:-1])

        # if address == 0x01 or address == 0x81 :
        #     printablePayload, separator, commandPayload = payload.rpartition(b'\x03')
        global keypadState
        global programmerState
        if declaredChecksum == computedChecksum: 
            # if address == 0x64 and len(payload) == 1:
            #     # response = makePacket(0x64 + 0x80, b'\x06\x05\x04\x03\x02\x01\x0e')
            #     # response = makePacket(0x64 + 0x80, b'\x0e')
            #     # response = makePacket(0x64 + 0x80, b'\x09\x09\x0e')
            #     # response = makePacket(0x65 + 0x80, b'\x01\x02\x05\x06\x08\x0e')
            #     response = makePacket(0x65 + 0x80, payload +  b'\x0e')
            #     serialPort.write(response)
            #     log(f"responding " + response.hex(" ") + "\n")
            keypadState.update(address, payload, packetChunks)
            programmerState.update(address, payload, packetChunks)



        fieldContentStrings = [
            # arrival time:
            f"{packetChunks[0].arrivalTime}", 

            #packet index:
            "packet  " + f"{packetCount:6d}",

            # number of chunks:
            f"{len(packetChunks):2d} " + "chunks",

            #checksum:
            "checksum " + (
                bytes((declaredChecksum,)).hex() + " OK " 
                if declaredChecksum == computedChecksum 
                else bytes((declaredChecksum,)).hex() + "!=" + bytes((computedChecksum,)).hex() ),
            
            # length:
            f"payloadAndChecksum length 0x{len(payload)+1:002x}",

            # byte 0:
            f"address 0x{address:002x}" ,

            "payload " + payload.hex(" "),

            
            # upperBitReadoutString:
            bytesToHighTwoBitReadableString(payload),

            #radxPrintableString:
            bytesToPrintableRADXString(payload),



        ]

        # fieldContentStrings :Sequence[str] = [rawHexStringOfAllExceptChecksum, upperBitReadoutString, radxPrintableString]
        #update the global fieldLengths according to the lengths of these fields.
        global fieldLengths
        fieldLengths = list(
            map(max, 
                itertools.zip_longest(
                    fieldLengths, 
                    map(len, fieldContentStrings),
                    fillvalue=0
                )
            )
        )

        paddedFields = map(
            lambda contentString, fieldLength: contentString.ljust(fieldLength),
            fieldContentStrings,
            fieldLengths
        )



        log(""
            # + bytes(packetBytes[2:-1]).hex(" ")
            # + "".join(map(byteToHumanReadableField, packetBytes[2:-1]))
            + " | ".join(paddedFields)
            + "\n"
        )

        # updateStaticOutputLine(" | ".join(paddedFields))


    def handleGarbage(garbageBytes: Sequence[int], garbageChunks : Sequence[Chunk]):
        # we might consider inspecting the garbage in search of packets that have a valid length and checksum, but an unknown start byte.
        global garbageCount
        garbageCount += 1
        log(
            f"{garbageChunks[0].arrivalTime}" + ": "
            + "garbage " + f"{garbageCount : 6d}" + ": "
            + f"{len(garbageChunks) : 2d} " + "chunks" + ": "
            + bytes(garbageBytes).hex(" ")
            + "\n"
        )
        
    clearScreen()
    while True:
        handleChunk(Chunk(serialPort.read_all()))

        # if receivedBytes:
        #     handleChunk(receivedBytes, timeOfThisIteration)
            
        #     iterationIndexOfThisReception = iterationIndex
        #     timeOfThisReception = timeOfThisIteration
        #     if(timeOfThisReception - timeOfLastReception > interClusterThresholdInterval):
        #         # in this case, a new cluster has begun.
        #         #wrap-up the last cluster:
        #         log("\n")
        #         if len(thisCluster)>=7:
        #             receivedChecksum = thisCluster[-5] 
        #             receivedStartByte = thisCluster[0] # always 0x01
        #             receivedDeclaredLengthOfPayloadAndPacketChecksum = thisCluster[1]
        #             payload = thisCluster[2:-5]
        #             expectedChecksum = checksum(thisCluster[0:-5])
                    
        #             if(receivedChecksum == expectedChecksum):
        #                 checksumPassed = True
        #                 pass
        #                 # print("good checksum!!!!  HOORAY!")
        #             else:
        #                 checksumPassed = False
        #                 checksumFailureCount += 1
        #                 pass
        #                 # print("checksum mismatch.")
                    
                    
        #             if len(payload) > 3:k
        #                 pass
        #                 # log("\t payload ascii: " + ''.join(map(lambda x: chr(x & 0x7f), payload)) + "\n")

        #             initialChangeCount = state._changeCount
        #             if checksumPassed:
        #                 if (receivedDeclaredLengthOfPayloadAndPacketChecksum > 4):
        #                     if 0x30 <= payload[0] <= 0x39:
        #                         state.length12AndStartsWithDigit = payload
        #                     else:
        #                         state.otherLengthGreaterThanFour = payload
        #                 else:
        #                     state.lengthLessThanOrEqualToFour = payload

        #                 if state._changeCount != initialChangeCount:
        #                     changeLog(f"{state._timeOfLastChange} {checksumFailureCount :7d} {clusterIndex :7d}: {state.getReportString()}\n")
        #         # possibly, print some statistics (number and time of receptions making up the cluster, a decoding of the cluster, etc.) the last cluster and display a
        #         # we might consider confirming that a healthy number of iterations
        #         # has occured within this cluster, to detect the case of an
        #         # iteration time that is not much smaller than the cluster interval,
        #         # in which case we are not really saying very much with our cluster
        #         # divisions.

        #         #initialize the new cluster:
        #         clusterIndex += 1
        #         numberOfReceptionsInThisCluster = 0 # to be incremented immediately below in the common reception-handling code
        #         thisCluster = []
        #         # print(f"{timeOfThisReception} {iOfThisReception - iOfLastReception :6d}: {receivedBytes.hex( ' ' )}")
        #         log(f"{timeOfThisReception}: ")
            
        #     thisCluster += receivedBytes



        #     numberOfReceptionsInThisCluster += 1
        #     log(receivedBytes.hex(" ") + " ")

        #     if thisCluster[-4:] == [0x64, 0x02, 0x09, 0x90]:
        #         # serialPort.write(bytes([0x64, 0x02, 0x09, 0x90]))
        #         # log(" responding ")
        #         pass

        #     #preparing to continue:
        #     iterationIndexOfLastReception = iterationIndexOfThisReception
        #     timeOfLastReception = timeOfThisReception
        # iterationIndex += 1

    logFile.close()
    changeLogFile.close()