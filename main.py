from typing import Iterable, List, Sequence
import typing
import serial;
import datetime;
import pathlib;


pathOfLogDirectory = pathlib.Path(__file__).parent.joinpath("logs")
pathOfLogDirectory.mkdir(exist_ok=True)
timeOfLaunch = datetime.datetime.now()
pathOfLogFile = pathOfLogDirectory.joinpath("log_{:%Y%m%d-%H%M}.txt".format(timeOfLaunch))
logFile = open(pathOfLogFile, 'a')

pathOfChangeLogFile = pathOfLogDirectory.joinpath("change_log_{:%Y%m%d-%H%M}.txt".format(timeOfLaunch))
changeLogFile = open(pathOfChangeLogFile, 'a')

def log(x: str):
    print(x,end='')
    logFile.write(x)

def changeLog(x: str):
    changeLogFile.write(x)
    changeLogFile.flush()

serialPort = serial.Serial(
    port='COM3', 
    baudrate=9600, 
    bytesize=serial.EIGHTBITS, 
    parity=serial.PARITY_NONE, 
    stopbits=serial.STOPBITS_ONE, 
    timeout=None, 
    xonxoff=False, rtscts=False, dsrdtr=False
)

def checksum(x: Iterable[int]) -> int:
    return (0xff - sum(x)) % 0x100


serialPort.reset_input_buffer()

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






state :State = State()

iterationIndex = 0
clusterIndex = 0
checksumFailureCount = 0
iterationIndexOfLastReception = 0
numberOfReceptionsInThisCluster = 0
thisCluster : List[int] = []
timeOfLastReception = datetime.datetime.now()
while True:
    timeOfThisIteration = datetime.datetime.now()
    receivedBytes = serialPort.read_all()
    if receivedBytes:
        iterationIndexOfThisReception = iterationIndex
        timeOfThisReception = timeOfThisIteration
        if(timeOfThisReception - timeOfLastReception > interClusterThresholdInterval):
            # in this case, a new cluster has begun.
            #wrap-up the last cluster:
            log("\n")
            if len(thisCluster)>=7:
                receivedChecksum = thisCluster[-5] 
                receivedStartByte = thisCluster[0] # always 0x01
                receivedDeclaredLengthOfPayloadAndPacketChecksum = thisCluster[1]
                payload = thisCluster[2:-5]
                expectedChecksum = checksum(thisCluster[0:-5])
                
                if(receivedChecksum == expectedChecksum):
                    checksumPassed = True
                    pass
                    # print("good checksum!!!!  HOORAY!")
                else:
                    checksumPassed = False
                    checksumFailureCount += 1
                    pass
                    # print("checksum mismatch.")
                
                
                if len(payload) > 3:
                    pass
                    # log("\t payload ascii: " + ''.join(map(lambda x: chr(x & 0x7f), payload)) + "\n")

                initialChangeCount = state._changeCount
                if checksumPassed:
                    if (receivedDeclaredLengthOfPayloadAndPacketChecksum > 4):
                        if 0x30 <= payload[0] <= 0x39:
                            state.length12AndStartsWithDigit = payload
                        else:
                            state.otherLengthGreaterThanFour = payload
                    else:
                        state.lengthLessThanOrEqualToFour = payload

                    if state._changeCount != initialChangeCount:
                        changeLog(f"{state._timeOfLastChange} {checksumFailureCount :7d} {clusterIndex :7d}: {state.getReportString()}\n")
            # possibly, print some statistics (number and time of receptions making up the cluster, a decoding of the cluster, etc.) the last cluster and display a
            # we might consider confirming that a healthy number of iterations
            # has occured within this cluster, to detect the case of an
            # iteration time that is not much smaller than the cluster interval,
            # in which case we are not really saying very much with our cluster
            # divisions.

            #initialize the new cluster:
            clusterIndex += 1
            numberOfReceptionsInThisCluster = 0 # to be incremented immediately below in the common reception-handling code
            thisCluster = []
            # print(f"{timeOfThisReception} {iOfThisReception - iOfLastReception :6d}: {receivedBytes.hex( ' ' )}")
            log(f"{timeOfThisReception}: ")
        
        thisCluster += receivedBytes



        numberOfReceptionsInThisCluster += 1
        log(receivedBytes.hex(" ") + " ")

        if thisCluster[-4:] == [0x64, 0x02, 0x09, 0x90]:
            # serialPort.write(bytes([0x64, 0x02, 0x09, 0x90]))
            log(" responding ")

        #preparing to continue:
        iterationIndexOfLastReception = iterationIndexOfThisReception
        timeOfLastReception = timeOfThisReception
    iterationIndex += 1

logFile.close()
changeLog.close()