import sys

KEYWORD = "alias"

if __name__ == "__main__":
    for fname in sys.argv[1:]:
        f = open(fname)
        lines = f.readlines()
        for i in range(len(lines)):
            line = lines[i]
            if line.startswith(KEYWORD):
                components = line.split()
                if len(components) != 3:
                    print >> sys.stderr, ('invalid syntax: ' + line)
                    sys.exit(1)
                orig = components[1]
                new  = components[2]
                for j in range(i+1, len(lines)):
                    lines[j] = lines[j].replace(orig, new)
        for line in lines:
            if line.startswith('alias'):
                continue
            print line,
