import uuid
import hashlib
import datetime
import pexpect
import re
import os
import csv

timeout_time = 60.0
iterations = 10

def machine_id():
    return ["%X" % (uuid.getnode())]

def exe_id():
    m = hashlib.md5()
    f = open("Main.native", "rb")
    d = f.read()
    f.close()
    m.update(d)
    return [m.hexdigest()]

def timestamp():
    return [datetime.datetime.utcnow().isoformat()]

def header(config, fname):
    return machine_id() + exe_id() + timestamp() + [config, fname]

def flag_error(config, fname):
    print "\033[91mError: %s -- %s\033[0m" % (config, fname)

def report_strace(config, fname, is_timeout, runtime, output):
    with open("data/results_strace.csv","a") as csvfile:
        sat = re.search(r"sat: ([^\s/]+)/([^\s]+)", output)
        le  = re.search(r"le : ([^\s/]+)/([^\s]+)", output)
        top = re.search(r"top: ([^\s/]+)/([^\s]+)", output)
        bot = re.search(r"bot: ([^\s/]+)/([^\s]+)", output)

        hdr = header(config, fname) + [runtime, is_timeout] 
        satt = [0, 0]
        let = [0, 0]
        topt = [0, 0]
        bott = [0, 0]

        if ((sat == None or le == None or top == None or bot == None) and not is_timeout) or runtime == None:
            flag_error(config, fname)
            return
        elif is_timeout:
            pass
        else:
            satt = [int(sat.group(1)), int(sat.group(2))]
            let = [int(le.group(1)), int(le.group(2))]
            topt = [int(top.group(1)), int(top.group(2))]
            bott = [int(bot.group(1)), int(bot.group(2))]

        csvwriter = csv.writer(csvfile)
        csvwriter.writerow(hdr + satt + let + topt + bott)



def report_sdsl(config, fname, is_timeout, runtime, output):
    with open("data/results_sdsl.csv","a") as csvfile:

        if runtime == None:
            flag_error(config, fname)
            return

        results = {}
        for mat in re.finditer(r"\((\d+)\) : PASS", output):
            results[int(mat.group(1))] = True

        for mat in re.finditer(r"\((\d+)\) : FAIL", output):
            results[int(mat.group(1))] = False

        pass_count = 0
        total_count = 0
        for (test,is_pass) in results.iteritems():
            total_count += 1
            if is_pass:
                pass_count += 1

        hdr = header(config, fname) + [runtime, is_timeout] 

        dat = [pass_count, total_count]
        csvwriter = csv.writer(csvfile)
        csvwriter.writerow(hdr + dat)

def run_test(config, fname):
    command = "./Main.native %s -time -eval tests/%s" % (config, fname)
    print command
    (outp,status) = pexpect.run(command, timeout=timeout_time, withexitstatus=True)

    is_timeout = False

    if status == None:
        # timeout
        is_timeout = True
        runtime = timeout_time
    else:
        res = re.search(r"Analysis time: ([^ ]+) seconds", outp)
        if res == None:
            runtime = None
        else:
            runtime = float(res.group(1))

    if fname.endswith(".sdsl"):
        report_sdsl(config, fname, is_timeout, runtime, outp)
    elif fname.endswith(".strace"):
        report_strace(config, fname, is_timeout, runtime, outp)
    else:
        raise "Error: Unknown benchmark file type"



def main ():
    configs = open("data/configurations").readlines()
    configs = [ config.strip() for config in configs ]
    configs = [ config for config in configs if config != "" ]
    configs = [ config for config in configs if config[0] != "#" ]

    tests = next(os.walk("tests"))[2]
    tests = [ test for test in tests if test.endswith(".sdsl") or test.endswith(".strace") ]
    #tests = [ test for test in tests if test.endswith(".strace") ]

    for test in tests:
        for config in configs:
            for i in xrange(iterations):
                run_test(config, test)

main()
