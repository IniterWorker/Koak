import enum

class Color(enum.Enum):
    RED = '\033[31;1m'
    YELLOW = '\033[93;1m'
    BLUE = '\033[34;1m'
    GREEN = '\033[32;1m'
    NORMAL = '\033[00m'

    def __str__(self):
        return self.value

def print_color(color, text, end='\n'):
    print(str(color) + text + str(Color.NORMAL), end=end)