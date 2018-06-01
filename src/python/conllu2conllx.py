#!/usr/bin/env python3


import collections
import sys
import util


if __name__ == '__main__':
    for block in util.blocks(sys.stdin):
        lines = block.splitlines()
        assert lines[-1].rstrip() == ''
        lines = lines[:-1]
        # Maps to fill:
        wordid_tokenid_map = {} # which word belongs to which multiword token
        oldid_newid_map = {'0': '0'} # how word/token ids change
        tokenid_headids_map = collections.defaultdict(list) # heads of words per multiword token (should be 1)
        newid = 0
        # First pass: fill the maps
        for line in lines:
            if line.startswith('#'):
                continue
            fields = line.split('\t')
            lineid = fields[0]
            # If line represents a multiword token, map all words belonging to it to it:
            if '-' in lineid:
                tokenid = lineid
                start, end = tokenid.split('-')
                wordids = [str(i) for i in range(int(start), int(end) + 1)]
                for wordid in wordids:
                    wordid_tokenid_map[wordid] = tokenid
            # Multiword tokens and words not belonging to multiword tokens are counted, increment the new ID:
            if not lineid in wordid_tokenid_map:
                newid += 1
                oldid_newid_map[lineid] = str(newid)
            # For words belonging to multiword tokens, we store their heads and throw the rest away:
            if lineid in wordid_tokenid_map:
                tokenid = wordid_tokenid_map[lineid]
                headid = fields[6]
                tokenid_headids_map[tokenid].append(headid)
        # Second pass: output the changed dependencies
        for line in lines:
            if line.startswith('#'):
                print(line)
                continue
            fields = line.split('\t')
            lineid = fields[0]
            # Throw words belonging to multiword tokens away:
            if lineid in wordid_tokenid_map:
                continue
            # Get the head. For multiword tokens, we have to look in tokenid_headids_map:
            headid = fields[6]
            if headid == '_':
                headids = tokenid_headids_map[lineid]
                headid = headids[0] # HACK!!! Is there a better way of choosing the head?
                #assert len(headids) == 1
            # If the head is in a multiword token, change ID to the ID of the multiword token:
            if headid in wordid_tokenid_map:
                headid = wordid_tokenid_map[headid]
            # Translate old IDs into new IDs:
            lineid = oldid_newid_map[lineid]
            headid = oldid_newid_map[headid]
            # Output:
            fields = [lineid,] + fields[1:6] + [headid,] + fields[7:]
            print('\t'.join(fields))
        print()

# deps within token: suppress
# deps from token: substitute tokenid for wordid
# deps to token: substitute tokenid for wordid
