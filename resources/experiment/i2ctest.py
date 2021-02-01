
import sys
import time
from pymata4 import pymata4

def arduino(my_board):
    # device address = 04
    my_board.set_pin_mode_i2c()

    my_board.i2c_write(4, [6, 0])
    time.sleep(.1)

print("ok here")

board = pymata4.Pymata4()

print("ok")

try:
    arduino(board)
    
except KeyboardInterrupt:
    board.shutdown()
    sys.exit(0)
