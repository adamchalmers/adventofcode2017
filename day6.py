from collections import defaultdict

EASY = [0,2,7,0]
HARD = [4,1,15,12,0,9,9,5,5,8,7,3,14,5,12,3]

def redist(banks):

	def inc(i):
		return (i+1) % len(banks)
		
	num_blocks = max(banks)
	i = banks.index(num_blocks)
	banks[i] = 0
	while num_blocks:
		i = inc(i)
		num_blocks -= 1
		banks[i] += 1

def redist_till_unseen(banks):
	time_seen = {}
	curr = banks
	cycles = 0
	while True:
		cycles += 1
		curr_t = tuple(curr)
		if curr_t not in time_seen:
			time_seen[curr_t] = cycles
		else:
			return cycles - time_seen[curr_t]
		redist(curr)

if __name__ == "__main__":
	print(redist_till_unseen(HARD))
