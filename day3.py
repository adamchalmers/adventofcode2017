from collections import defaultdict 

def grow_square(square):
	"""Turns a spiral-square with width n into a spiral-square with width n+1.
	square[i] is the coordinates of the square labelled (i+1) in the below diagram:
		17  16  15  14  13
		18   5   4   3  12
		19   6   1   2  11
		20   7   8   9  10
		21  22  23---> ...
	"""

	d = len(square)**0.5
	if int(d) != d:
		raise f"Square passed with length {len(square)}"
	d = int(d) + 2

	# Add a new leftmost column
	(last_x, last_y) = square[-1]
	for i in range(d - 1):
		square.append((last_x + 1, last_y + i))

	# Add a new topmost row
	(last_x, last_y) = square[-1]
	for i in range(1, d):
		square.append((last_x - i, last_y))

	# Add a new leftmost column
	(last_x, last_y) = square[-1]
	for i in range(1, d):
		square.append((last_x, last_y - i))

	# Add a new bottom row
	(last_x, last_y) = square[-1]
	for i in range(1, d):
		square.append((last_x + i, last_y))

def sum_around(d, point):
	"""Sum of all points around the given point"""
	(x, y) = point
	points = [ 
		(x+1, y),
		(x-1, y), 
		(x, y+1), 
		(x, y-1), 
		(x+1, y+1), 
		(x+1, y-1), 
		(x-1, y+1), 
		(x-1, y-1), 
	]
	return sum([d[point] for point in points])


def soln1(n):
	(x,y) = locate(n)
	return abs(x) + abs(y)

def soln2(n):
	d = defaultdict(lambda: 0)
	d[(0,0)] = 1
	square = square_containing(n)

	for point in square[1:]:
		d[point] = sum_around(d, point)
		if d[point] > n:
			return d[point]

def locate(n):
	return square_containing(n)[n-1]

def square_containing(n):
	square = SQUARE_WIDTH_1()
	while len(square) < n:
		grow_square(square)
	return square

def main():
	test_small()
	test_med()
	test_containing()
	input_num = 312051
	print(f"Soln 1: {soln1(input_num)}")
	print(f"Soln 2: {soln2(input_num)}")

def test_containing():
	assert square_containing(1) == SQUARE_WIDTH_1()
	assert square_containing(2) == SQUARE_WIDTH_3()
	assert square_containing(8) == SQUARE_WIDTH_3()
	assert square_containing(9) == SQUARE_WIDTH_3()
	assert square_containing(10) == SQUARE_WIDTH_5()

def test_small():
	square = SQUARE_WIDTH_1()
	grow_square(square)
	assert square == SQUARE_WIDTH_3(), square

def test_med():
	square = SQUARE_WIDTH_3()
	grow_square(square)
	assert square == SQUARE_WIDTH_5()

def SQUARE_WIDTH_1(): return [(0,0)]
def SQUARE_WIDTH_3(): return [(0,0), (1,0), (1,1), (0,1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]
def SQUARE_WIDTH_5(): return [(0, 0), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1), (2, -1), (2, 0), (2, 1), (2, 2), (1, 2), (0, 2), (-1, 2), (-2, 2), (-2, 1), (-2, 0), (-2, -1), (-2, -2), (-1, -2), (0, -2), (1, -2), (2, -2)]

if __name__ == "__main__":
	main()
