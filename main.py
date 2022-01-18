from typing import Any, Dict, Iterable, List, Optional, Sequence
import typing
import serial;
import datetime;
import pathlib;
import itertools;
import sys;
# import curses
import re


# thanks to https://stackoverflow.com/questions/2408560/non-blocking-console-input
# for figuring out how to effectively read stdin in a non-blocking manner:
import threading
import queue
consoleInputQueue = queue.Queue()
# sys.stdin.reconfigure(line_buffering=False, write_through=True) # these settings don't seem to have any effect -=- the line-buffering might be being performed bny the terminal.
def collectConsoleInput():
    global consoleInputQueue
    while True:
        # consoleInputQueue.put(sys.stdin.read(1))
        consoleInputQueue.put(sys.stdin.readline())
inputThread = threading.Thread(target=collectConsoleInput)
inputThread.daemon = True
inputThread.start()


pathOfLogDirectory = pathlib.Path(__file__).parent.joinpath("logs")
pathOfLogDirectory.mkdir(exist_ok=True)
timeOfLaunch = datetime.datetime.now()
# pathOfLogFile = pathOfLogDirectory.joinpath("log_{:%Y%m%d-%H%M}.txt".format(timeOfLaunch))
pathOfLogFile = pathOfLogDirectory.joinpath("log.txt")
if pathOfLogFile.is_file():
    pathOfLogFile.rename(pathOfLogDirectory.joinpath("log_archived_{:%Y%m%d-%H%M%S}.txt".format(timeOfLaunch)))


# pathOfChangeLogFile = pathOfLogDirectory.joinpath("change_log_{:%Y%m%d-%H%M}.txt".format(timeOfLaunch))
pathOfChangeLogFile = pathOfLogDirectory.joinpath("change_log.txt")
if pathOfChangeLogFile.is_file():
    pathOfChangeLogFile.rename(pathOfLogDirectory.joinpath("change_log_archived_{:%Y%m%d-%H%M%S}.txt".format(timeOfLaunch)))


# pin 4 of the programming port (UART data out of control panel (apparently
# open-drain)) carries the exact same signal that the control panel transmits on
# the SDI bus, but is not simply a copy of the SDI bus signal in that it does
# not carry the signal transmitted by other devices besides the panel.  This
# means, for instance, that pin 4 of the programming port is not suitable for
# sniffing data sent by devices on the SDI bus other than the control panel
# itself; if you want to sniff all SDI traffic, it is necessary to connect to
# the SDI bus itself.  
# 
# It seems that everything that the control panel transmits on the SDI bus shows
# up on port 4 of the programming port, but is the opposite true?  Does
# everything the control panel transmits on port 4 of the programming port also
# show up on the SDI bus?  I would guess that the answer is yes.

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
        "\N{escape}[" + str(lineNumber) + ";1H"  # place the cursor at the beginning of the specified line
        + x
        + "\N{escape}[K", # clear the remainder of the line
        end=''
    )
    sys.stdout.flush()

def clearScreen():
    print(
        "\N{escape}[1;1H"
        "\N{escape}[0J",
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
    return x in (0x01, 0x64, 0x81, 0x00, 0x80)

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
        self.updateCount    : int = 0


        self.outputLineNumber = outputLineNumber
    def getReportString(self) -> str:
        return self._reportString

    def update(self, addressAndDirection: int, payload: bytes(), chunks: Optional[Sequence['Chunk']] = None) -> None:
        if (addressAndDirection & 0x7f) == self.address :
            self.lastUpdateTime = (chunks[0].arrivalTime if chunks else datetime.datetime.now())
            self.updateCount += 1
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
        # self.nextResponsePayload = b'\x00\x00\x0e'
        self.nextTransmissionArguments = [0x64 + 0x80, 0]
        self.lastTransmission : Optional[bytes] = None
        self.lastTransmissionThatElicitedUnusualResponse  : Optional[bytes] = None
        self.responseInterval = 100


    def update(self, addressAndDirection: int, payload: bytes(), chunks: Optional[Sequence['Chunk']] = None):
        if (addressAndDirection & 0x7f) == self.address :
            if addressAndDirection & 0x80:
                # in this case, we are seeing a packet that was sent BY the device, not TO the device.
                pass
            # else:
            #     if payload == b'\x09':
                    
            #         if self.updateCount % self.responseInterval == 0:
            #             # response = makePacket(self.address + 0x80, self.nextResponsePayload +  b'\x0e')
            #             # response = makePacket(self.address + 0x80, b'\x02\x05\x06' +  b'\x0e')
            #             thisTransmissionArguments = self.nextTransmissionArguments
                        
                        
            #             transmissionAddress = thisTransmissionArguments[0]
            #             transmissionPayload = thisTransmissionArguments[1].to_bytes(2, byteorder='big')[1:]
                        
            #             # transmissionPayload = b'\x06\x05\x04\x03\x02\x01\x0e'
            #             # transmissionPayload = b''
            #             # response = makePacket(transmissionAddress, transmissionPayload)
            #             self.transmit(destinationAddress = transmissionAddress, payload=transmissionPayload)
                        
            #             self.nextTransmissionArguments

            #             if thisTransmissionArguments[1] == 0xffff:
            #                 updateStaticOutputLine(f"{datetime.datetime.now()}: cycle complete",self.outputLineNumber+4)
            #                 self.nextTransmissionArguments[0] = (thisTransmissionArguments[0] + 1) & 0xff
            #             self.nextTransmissionArguments[1] = (thisTransmissionArguments[1] + 1) & 0xffff

            #             # response = makePacket(0x64 + 0x80, b'\x06\x05\x04\x03\x02\x01\x0e')
            #             # response = makePacket(0x64 + 0x80, b'\x0e')
            #             # response = makePacket(0x64 + 0x80, b'\x09\x09\x0e')
            #             # response = makePacket(0x65 + 0x80, b'\x01\x02\x05\x06\x08\x0e')

                        
            #             # global serialPort
            #             # serialPort.write(response); serialPort.flushOutput()
            #             # message = f"{datetime.datetime.now()}: " + "transmitting " + response.hex(" ")
            #             # self.lastTransmission = response
            #             # updateStaticOutputLine(message,self.outputLineNumber+1)
            #             # log(message + "\n")


            #     else:
                    
            #         thisTransmissionThatElicitedUnusualResponse = self.lastTransmission


            #         message = (""
            #             + f"{datetime.datetime.now()}: programmer {hex(self.address)} received an unusual payload: " + payload.hex(" ") 
            #             + ". lastTransmission: " 
            #             + ("*" if thisTransmissionThatElicitedUnusualResponse != self.lastTransmissionThatElicitedUnusualResponse else " ")
            #             + ("None" if self.lastTransmission is None else self.lastTransmission.hex(" "))
            #         )
            #         # regex for searching the log: (?<= received an unusual payload: (?!02))([\dabcdef]{2})
            #         # (?<= received an unusual payload: (?!02))(..)
            #         # I\r\n\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d.\d\d\d\d\d\d \| 
            #         # (?<=address (?!(0x01|0x64)))(....)

            #         updateStaticOutputLine(message,self.outputLineNumber+3)
            #         log(message + "\n")

            #         self.lastTransmissionThatElicitedUnusualResponse  = thisTransmissionThatElicitedUnusualResponse
                    
                    
                    
            super().update(addressAndDirection, payload, chunks)        
        # look for any characters on stdin (which represent keystrokes input by the user), and 
        # transmit accordingly.
        

        # userInput : Optional[str] = sys.stdin.buffer.read(1)
        userInput : str = ""
        # while not consoleInputQueue.empty():
        #     userInput += consoleInputQueue.get()
        if not consoleInputQueue.empty():
            userInput += consoleInputQueue.get()
        if userInput:
            userInputLines = userInput.split("\n")
            firstLineContent = userInputLines[0]
            

            # re.split()..., below, returns a list wherein the odd-indexed elements are our strings matching the pattern, and the interstitial strings are at the even indices.
            encodedPayload : List[int] = []
            for i, value in enumerate(re.split(r'(\\x[\dabcdef]{2})', firstLineContent, flags=re.IGNORECASE)):
                if i % 2 == 0:
                    encodedPayload += map(
                        lambda x: x & 0x3f,
                        value.encode('ascii')
                    )
                else:
                    encodedPayload.append(int(value[2:], base=16))
            
            payloadBytes : bytes = bytes(encodedPayload)
            # payloadBytes = firstLineContent.encode('unicode_escape')
            # updateStaticOutputLine(f"{datetime.datetime.now()}: " + "input line 0: " + firstLineContent + " raw bytes: " + userInput.encode("ascii").hex(" ") , self.outputLineNumber+1)
            updateStaticOutputLine(f"{datetime.datetime.now()}: " + "input line 0: " + firstLineContent + " payloadBytes: " + payloadBytes.hex(" ") + " " +  "printableRADXString: " + bytesToPrintableRADXString(payloadBytes), self.outputLineNumber+1)
            self.transmit(payloadBytes, rawMode=False)
        # if userInput:
        #     updateStaticOutputLine(f"{datetime.datetime.now()}: " + "input: " + userInput, self.outputLineNumber+1)
        # else:
        #     updateStaticOutputLine(f"{datetime.datetime.now()}: " + " no input")
        


        if not consoleInputQueue.empty():
            userInput : str = consoleInputQueue.get()
            

    def transmit(self, payload : bytes, destinationAddress : Optional[int] = None, rawMode=False) -> None:
        _destinationAddress = (self.address + 0x80 if destinationAddress is None else destinationAddress)
        packet = (payload if rawMode else makePacket(_destinationAddress, payload) )    

        message = f"{datetime.datetime.now()}: " + "transmitting " + packet.hex(" ")           
        global serialPort
        serialPort.write(packet); serialPort.flushOutput()
        
        updateStaticOutputLine(message,self.outputLineNumber+2)
        log(message + "\n")
        self.lastTransmission = packet

class Chunk:
    lastChunkIndex : int = -1
    def __init__(self, byteArray : Iterable[int]):
        self.arrivalTime = datetime.datetime.now()
        self.byteArray = byteArray
        self.index = self.lastChunkIndex + 1
        # self.index = Chunk.lastChunkIndex + 1
        self.lastChunkIndex = self.index


# logFile
# changeLogFile
# serialPort
keypadState : KeypadState
device100State  : KeypadState
programmerState  : ProgrammerState
lastNonemptyChunk : Optional[Chunk]
clusterCount : int
checksumFailureCount : int
packetCount : int
garbageCount : int
bufferOfNonemptyChunks : List[Chunk]
fieldLengths : List[int]
packetBytes : List[int] 
packetChunks : List[Chunk] # the set of chunks from which packetBy
garbageBytes : List[int] 
garbageChunks : List[Chunk]  # the set of chunks from which ga
weAreReadingAPacket : bool 
targetLengthOfThisPacketIsKnown : bool 
targetLengthOfThisPacket : int
logFile : Any
changeLogFile : Any
serialPort : Any


doDetectClusters : bool = False


def main() -> None:
    global logFile
    global changeLogFile
    global serialPort
    global keypadState
    global device100State
    global programmerState
    global lastNonemptyChunk
    global clusterCount
    global checksumFailureCount
    global packetCount
    global garbageCount
    global bufferOfNonemptyChunks
    global fieldLengths
    global packetBytes 
    global packetChunks 
    global garbageBytes  
    global garbageChunks 
    global weAreReadingAPacket 
    global targetLengthOfThisPacketIsKnown
    global targetLengthOfThisPacket
    global doDetectClusters
    global pathOfLogFile
    global pathOfChangeLogFile

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


    keypadState  = KeypadState(address = 0x01, outputLineNumber=3)
    device100State  = KeypadState(address = 0x64, outputLineNumber=8)
    programmerState = ProgrammerState(address = 0x01, outputLineNumber=13)
    lastNonemptyChunk = None
    clusterCount = 0
    checksumFailureCount = 0
    packetCount = 0
    garbageCount = 0
    bufferOfNonemptyChunks  = []
    fieldLengths = []




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
        global garbageBytes 
        global garbageChunks 

        global packetBytes 
        global packetChunks 

        global weAreReadingAPacket 
        global targetLengthOfThisPacketIsKnown
        global targetLengthOfThisPacket 

        garbageBytes  = []
        garbageChunks  = [] # the set of chunks from which garbageBytes comes
        weAreReadingAPacket  = False



        
        # we regard the cluster as consisting of an alternating sequence of garbage, packet, garbage, ..., packet, garbage
        # it is distinctly possible that garbageBytes might be part of a real packet, or a packet type that we do not know about, 
        # so we record garabage for later analysis.
        for thisByte, chunk in ( (thisByte, chunk) for chunk in chunks for thisByte in chunk.byteArray):  
            processInputByte(thisByte, chunk)
        # clean-up:
        if weAreReadingAPacket:
            # yikes: we have hit the end of the cluster without finsihing our packet
            # (i.e. without reaching the declared length of the packet, or possibly without even having a length declared at all (i.e. only having received the startByte) -- therefore
            # the packet is probably malformed, and we will treat it as garbage.
            if packetBytes: handleGarbage(packetBytes, packetChunks)
        else:
            if garbageBytes: handleGarbage(garbageBytes, garbageChunks)

    

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
        global device100State
        if declaredChecksum != computedChecksum: 
            global checksumFailureCount
            checksumFailureCount += 1


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

        if declaredChecksum == computedChecksum: 
            keypadState.update(address, payload, packetChunks)
            device100State.update(address, payload, packetChunks)
            programmerState.update(address, payload, packetChunks)

        updateStaticOutputLine(f"packetCount: {packetCount:6d}.  checksumFailureCount: {checksumFailureCount:6d}", lineNumber=1)


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
    serialPort.reset_input_buffer()
    

    garbageBytes  = []
    garbageChunks  = [] 
    weAreReadingAPacket = False

    
    def processInputByte(thisByte : int, chunk: Chunk):
        global weAreReadingAPacket
        global garbageBytes
        global garbageChunks
        global packetBytes
        global packetChunks
        global targetLengthOfThisPacketIsKnown
        global targetLengthOfThisPacket

        if (not weAreReadingAPacket) and isStartByte(thisByte):
            # switch to packet-reading mode, first handling any accumulated garbage.
            if garbageBytes: 
                handleGarbage(garbageBytes, garbageChunks); 
                garbageBytes = []
                garbageChunks = []

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



    while True:
        chunk = Chunk(serialPort.read_all())
        
        if doDetectClusters:
            handleChunk(Chunk(serialPort.read_all()))
        else:
            for thisByte in chunk.byteArray:
                processInputByte(thisByte, chunk)
            
        # if weAreReadingAPacket:
        #     # yikes: we have hit the end of the chunk without finsishing our
        #     # packet this is not indicative of any problem -- there is no
        #     # guarantee that our serial port input buffer is synchronized with
        #     # packet boundaries -- but it might be intersting to note.
        #     pass
        # else:
        #     pass

    
 




 
    # to do: put these clean-up statements in a signal handler so they will actually by called:
    logFile.close()
    changeLogFile.close()

if __name__ == "__main__":
    main()