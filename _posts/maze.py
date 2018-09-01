import matplotlib.pyplot as plt
import numpy as np
import random

GRID = np.zeros([50, 50])

MAX_TUNNEL_WIDTH = 4


class Rect(object):

    def __init__(self, x, y, width, height):
        self.x = x
        self.y = y
        self.width = width
        self.height = height

    def fill(self, value):
        GRID[self.y:self.y + self.height, self.x:self.x + self.width] = value

    def wall(self, value):
        for wall in self.iter_walls():
            wall.fill(value)

    def intersects(self, other):
        # TODO(lambdaviking): Decide about -1 here.
        x_intersects = self._range_overlap(self.x, self.x + self.width, other.x, other.x + other.width)
        y_intersects = self._range_overlap(self.y, self.y + self.height, other.y, other.y + other.height)
        return x_intersects and y_intersects

    def 

    def iter_walls(self):
        yield Rect(self.x, self.y, self.width, 1)
        yield Rect(self.x, self.y, 1, self.height)
        yield Rect(self.x, self.y + self.height - 1, self.width, 1)
        yield Rect(self.x + self.width - 1, self.y, 1, self.height)

    @staticmethod
    def _range_overlap(a_min, a_max, b_min, b_max):
        return not (a_min > b_max or b_min > a_max)


def rand_index(min_index, max_index, xvals, obstacles):
    while True:
        index = random.randint(min_index, max_index)
        if not any(obs.intersects(Rect(:
            return index


def flip(p=.5):
    return random.random() < p


def gen_maze(rect, enter, exit):

    rect.wall(1)
    enter.fill(0)
    exit.fill(0)

    if rect.width <= MAX_TUNNEL_WIDTH or rect.height <= MAX_TUNNEL_WIDTH:
        return

    if flip(): # Split vertically.
        split_index = rand_index(rect.y + 1, rect.y + rect.height - 2,
            xvals=[rect.x, rect.x + rect.width - 1],
            obstacles=[enter, exit])
        rects = []
    else:

if __name__ == "__main__":
    gen_maze(Rect(1, 1, 4, 30), Rect(0, 10, 2, 2), Rect(0, 20, 2, 2))
    plt.imshow(GRID)
    plt.show()