def block2tuples(block):
    tuples = []
    lines = block.rstrip().splitlines()
    for line in lines:
        fr, to, tokid, token = line.rstrip().split(' ', 3)
        fr = int(fr)
        to = int(to)
        tuples.append((fr, to, tokid, token))
    return tuples
