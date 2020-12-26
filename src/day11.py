import time
ti = time.time()
s = open("../input/11.txt").read()
lines = s.split('\n')
board = [[c for c in x] for x in lines]

# unlike the initial (natural) O(n*t) solution
# (where t is number of update loops; seems to be at least O(sqrt(n))),
# this runs in amortized O(n) in the input (empirically about 10x faster)

def get_neighbors(x, y, board):
    """
    part 2 "neighbors"
    complexity of running get_neighbors for every square can actually be shown
    to run in linear time (not O(n^2)) by considering the number of times each
    square (floor or nonfloor) is visited
    """
    nb = []
    for di in range(8):
        dj = (di+2) % 8
        dx, dy = (di % 4 > 0) * (2 * (di//4) - 1), (dj % 4 > 0) * (2 * (dj//4) - 1)
        n = 0
        while True:
            n += 1
            nx, ny = x + n*dx, y + n*dy
            if nx < 0 or nx >= len(board) or ny < 0 or ny >= len(board[0]):
                break
            if board[nx][ny] != '.':
                nb.append((nx, ny))
                break
    return nb

def update(x, y, board):
    """
    updates two steps at once
    '.' = floor
    '_' = permanently empty seat
    '#' = permanently filled seat
    'L' = undecided, alternating seat (currently empty)
    """
    if board[x][y] != 'L':
        return board[x][y]
    countf, countl = 0, 0
    for nx, ny in neighbors[x][y]:
        countf += board[nx][ny] == '#'
        countl += board[nx][ny] == 'L'
    if countf > 0:
        return '_'
    if countf + countl < 5:
        return '#'
    return 'L'

# creates and stores all neighbor data
neighbors = []
for x in range(len(board)):
    neighbors.append([])
    for y in range(len(board[x])):
        neighbors[x].append(get_neighbors(x, y, board))

# initializes frontier by looping through the entire board once
frontier = set()
unvisited = set()
for x in range(len(board)):
    for y in range(len(board[x])):
        new = update(x, y, board)
        if new != board[x][y]:
            frontier.add((x, y, new))
        if new == 'L':
            unvisited.add((x, y))

# bfs-like spreading of seats settling down ensures that each seat is only operated on O(1) times
# (once each for itself and its <=8 neighbors)
while frontier:
    for ox, oy, old in frontier:
        board[ox][oy] = old
    newfrontier = set()
    for ox, oy, old in frontier:
        for x, y in neighbors[ox][oy]:
            if (x, y) in unvisited:
                new = update(x, y, board)
                if new != board[x][y]:
                    newfrontier.add((x, y, new))
                    unvisited.remove((x, y))
    frontier = newfrontier              

print(''.join([''.join(x) for x in board]).count('#'))
print(time.time() - ti)
