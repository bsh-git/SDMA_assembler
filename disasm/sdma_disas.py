#!/usr/bin/env python3

import sys
import re
import getopt
import os

global labelSet

line_pat = re.compile(r'(\[[ 0-9.]*\] )?([0-9a-f]+): ([0-9a-f]+) ([0-9a-f]+) ([0-9a-f]+) ([0-9a-f]+)')

opcode1 = ("reserved", "ldi", "xori", "addi", "subi", "ori",
        "andni", "andi", "tsti", "cmpeqi", "ld", "st", 
        "ldf", "stf",
        "reserved", "loop", "reserved")

opcode2 = ("", "bclri", "bseti", "btsti")

opcode3 = ("reserved", "mov", "xor", "add", "sub", "or", "andn", "and", "tst", "cmpeq", "cmplt", "cmphs")

opcode4 = ("jmpr", "jsrr", "ldrpc", "reserved", 
        "reserved", "reserved", "reserved", "reserved", 
        "revb", "revblo", "rorb", "reserved",
         "ror1", "lsr1", "asr1", "lsl1",
          "???", "???", "???", "???", "???")

opcode5 = ("done", "notify")

opcode6 = ("bf", "bt", "bsf", "bdf")

sizename = ("SZ0", "SZ8", "SZ16", "SZ32")
def disass_one(addr, code):
    def jump(opc, target):
        l = labelSet.make_label(target)
        print("%s %s (0x%x)" % (opc, l.label, target))

    l = labelSet.get_label(addr)
    if l:
        print("%s:" % l.label)
    print("%04x: %04x\t" % (addr, code), end = "")

    if (code & 0xc000) == 0x8000:
        jump("jmp", (code & 0x3fff))
        return

    if (code & 0xc000) == 0xc000:
        jump("jsr", (code & 0x3fff))
        return

    if (code & 0xfc00) == 0x7c00:
        x = (code >> 8) & 0x03
        diff = code & 0xff
        if diff > 0x7f:
            diff = -256 + diff

        target = addr + diff + 1
        jump(opcode6[x], target)
        return

    if (code & 0xf800) == 0:
        x = (code >> 5) & 0x07
        if x == 7:
            if code == 0x06e2:
                print("cpShReg")
            else:
                print("reserved")
            return
        r = (code >> 8) & 0x07
        n = code & 0x1f
        if 1 <= x and x <= 3:
            print("%s r%d, %d" % (opcode2[x], r, n))
            return
        if x >= 4:
            x = (code >> 3) & 0x0f
            s = code & 0x07
            print("%s r%d, r%d" % (opcode3[x], r, s))
            return
        x = code & 0x1f
        if x >= 8:
            print("%s r%d" % (opcode4[x-8], r))
            return
        else:
            x = code & 0xff
            if x == 0 or x == 1:
                print("%s %d" % (opcode5[x], (code >> 8) & 0x07))
                return
            if code == 5:
                print("softBkpt")
                return
            if code == 6:
                print("ret")
                return
            if code == 0x0707:
                print("illegal")
                return
            if (code & 0xfcff) == 0x0007:
                flag = (code >> 8) & 0x03
                print("clrf %s (%d)" % (('SF|DF', 'DF', 'SF', 'none')[flag],flag))
                return

    n = code >> 11
    r = (code >> 8) & 0x07
    i = code & 0xff
    op = opcode1[n]

    if n == 0x0a or n == 0x0b:
       d = i >> 3
       b = i & 0x07
       print("%s r%d,(r%d,%d)" % (op, r, b, d))
    elif n == 0x0f:
       if (r & 0x04) != 0:
           print("reserved")
       else:
           loop_exit = addr + i + 1
           l = labelSet.make_label(loop_exit)
           print("loop %d, %s (.+1+%d=0x%x)" % (r, l.label, i, loop_exit))
    elif n == 12:
        ldf(r, i)
    elif n == 13:
        stf(r, i)
    else:
        print("%s r%d, %d  (0x%x)" % (op, r, i, i))

burst_regname = ('MSA', 'MDA', 'MD', 'MS')
size_name = ('SZ0', 'SZ8', 'SZ16', 'SZ32')


def ldf(r, i):
    opr = '???'
    if i & 0xc0 == 0x00:
        opr = ldf_burst(i)
    elif i & 0xc0 == 0xc0:
        opr = ldf_periph(i)
    elif i & 0xc0 == 0x40:
        opr = 'BP???'
    else:
        opr = 'CRC???'
    print("ldf r%d, %s (%02x)" % (r, opr, i))

def ldf_burst(a):
    reg = (a >> 2) & 0x03
    if reg != 2:
        return burst_regname[reg]
    opr = [burst_regname[reg]]
    if a & 0x20:
        opr.append('PF')
    opr.append(size_name[a & 0x03])
    return "|".join(opr)

def ldf_periph(a):
    if a & 0x3f == 0x3f:
        return 'PS'
    if a & 0x08:
        opr = ['PD']
        if a & 0x20:
            opr.append('PF')
        if a & 0x10:
            opr.append('CPY')
        return "|".join(opr)            
    elif a & 0x10:
        return 'PDA'
    else:
        return 'PSA'
        

def stf(r, i):
    if i & 0xc0 == 0x00:
        opr = stf_burst(i)
    elif i & 0xc0 == 0xc0:
        opr = stf_periph(i)
    elif i & 0xc0 == 0x40:
        opr = 'BP???'
    else:
        opr = 'CRC???'
    print("stf r%d, %s (%02x)" % (r, opr, i))

def stf_burst(a):
    opr = []
    reg = (a >> 2) & 0x03
    if reg == 0 and a & 0x20:
        opr.append('PF')                # prefetch
    if (reg == 0 or reg == 1) and a & 0x10:
        opr.append('FR')                # freeze
    if reg == 2:
        if a & 0x20:
            opr.append('FL')            # flush
        if a & 0x10:
            opr.append('CPY')           # copy
    if reg >= 2:
        opr.append(size_name[a & 0x03])
    return "|".join([burst_regname[reg]] + opr)

def stf_periph(a):
    if a & 0x3f == 0x3f:
        return 'PS'
    if a & 0x3f == 0x0c:
        return 'clrefPS'
    if a & 0x3f == 0x08:
        return 'PD'
    if a & 0x10:
        opr = ['PDA']
    else:
        opr = ['PSA']
        if a & 0x20:
            opr.append('PF')
    opr.append(size_name[a & 0x03])
    if a & 0x0c > 0:
        opr.append(('', 'I', 'D', 'U')[(a >> 2) & 0x03])
    return "|".join(opr)
    
    
class LabelSet:
    def __init__(self):
        self.labels = {}
        self.labels_rev = {}
        self.num = 0

    def get_label(self, addr):
        if addr in self.labels_rev:
            return self.labels_rev[addr]
        return None

    def make_label(self, addr):
        l = self.get_label(addr)
        if l:
            return l
        while True:
            n = "L%04d" % self.num
            if n not in self.labels:
                l = Label(addr, n)
                self.labels[n] = l
                self.labels_rev[addr] = l
                return l
            self.num = self.num + 1

    def write_labels(self, filename):
        f = open(filename, "w")
        for a in sorted(list(self.labels_rev.keys())):
            l = self.labels_rev[a]
            f.write("%04x\t%s\n" % (l.addr, l.label))
        f.close()

    def read_labels(self, filename):
        try:
            f = open(filename, "r")
        except FileNotFoundError:
            return
        for line in f:
            w = re.split(r'\s+', line)
            a = int(w[0], 16)
            l = Label(a, w[1])
            self.labels[w[1]] = l
            self.labels_rev[a] = l

            m = re.search(r'^L(\d+)$', w[1]) 
            if m:
                number = int(m.group(1), 10)
                if number >= self.num:
                    self.num = number + 1

class Label:
    def __init__(self, addr, label):
        self.addr = addr
        self.label = label

    def label(self):
        return self.label

    def addr(self):
        return self.addr

def main1():
    for line in sys.stdin:
        m = line_pat.match(line)
        if m:
            addr = m.group(2)
        addr = int(addr, 16)
        for d in m.group(3,4,5,6):
            d = int(d, 16)
            disass_one(2*addr, (d >> 16) & 0xffff)
            disass_one(2*addr + 1, d & 0xffff)
            addr = addr + 1

def main2(addr):
    for line in sys.stdin:
        line = re.sub(r',$', "", re.sub(r'\s', "", line))
        for w in re.split(r',', line):
            d = int(w, 0)
            disass_one(addr, d)
            addr = addr + 1

def disass(addr):
    input_fmt = None
    for line in sys.stdin:
        if not input_fmt:
            if line_pat.match(line):
                input_fmt = 1
            else:
                input_fmt = 2

        if input_fmt == 1:
            m = line_pat.match(line)
            if m:
                addr = m.group(2)
            addr = int(addr, 16)
            for d in m.group(3,4,5,6):
                d = int(d, 16)
                disass_one(2*addr, (d >> 16) & 0xffff)
                disass_one(2*addr + 1, d & 0xffff)
                addr = addr + 1
        else:
            line = re.sub(r',$', "", re.sub(r'\s', "", line))
            for w in re.split(r',', line):
                d = int(w, 0)
                disass_one(addr, d)
                addr = addr + 1
                

def main():
    global labelSet
    labelSet = LabelSet()
    base = 0

    label_file = None

    try:
        opts, args = getopt.getopt(sys.argv[1:], "a:l:")
    except getopt.GetoptError as err:
        print(err)
        usage()
        sys.exit(2)
    for o, a in opts:
        if o in ("-l", "--label"):
            label_file = a
            labelSet.read_labels(a)
        elif o in ("-a", "--addr=", "--address="):
            base = int(a, 16)
        else:
            assert False, "unhandled option"

    disass(base)

    if label_file:
        labelSet.write_labels(label_file)

if __name__ == "__main__":
    main()
