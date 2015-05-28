import sys
l = open(sys.argv[1]).readlines()
l = filter(lambda x: x[0] != 'c' and x[0] != 'C', l)
lcount = 1
closing = 0
q = {}
q["e"] = "exists"
q["a"] = "forall"
header = True
for l in l:
    if l[0] == 'c' or l[0] == 'C':
        # comment
        continue
    if l[0] == 'p':
        # header
        [p, cnf, maxv, lcount] = l.split(" ")
        lcount = int(lcount)
        vcount = int(maxv) + 1
        print "; vcount %d" % vcount
        print "; lcount %d" % lcount
        for v in xrange(1,int(vcount)):
            print "(declare-fun v%d () Bool)" % v
        print "(assert "
        closing += 1
        continue
    if l[0] == "e" or l[0] == "a":
        # quantifier
        print "(%s (" % q[l[0]]
        l = [ int(x) for x in l.split(' ')[1:] ]
        l = [ x for x in l if x > 0 ]
        for v in l:
            print "(v%d Bool)" % v
        print ")"
        closing += 1
        continue

    # clause
    if header:
        print "(and"
        closing += 1
        header = False

    if lcount == 0:
        print "; Warning, not all lines processed"
        break
    
    l = [ int(x) for x in l.split(' ') ]
    n = [ "(not v%d)" % -x for x in l if x < 0 ]
    p = [ "v%d" % x for x in l if x > 0 ]
    print "(or %s %s)" % (" ".join(n), " ".join(p))
    lcount -= 1

print ")"*closing
print "(check-sat)"
