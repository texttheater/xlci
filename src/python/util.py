def blocks(f):
    """Reads a file into a sequence of blocks.

    Takes a file-like object and returns its contents as a sequence of blocks
    terminated by empty lines."""
    block = ''
    for line in f:
        block += line
        if line == '\n':
            yield block
            block = ''
    if block:
        yield block
