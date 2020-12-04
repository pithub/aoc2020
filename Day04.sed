s/byr:([0-9][0-9][0-9][0-9])/, Byr \1/
s/byr:[^ ]+/, Byr 0/
s/iyr:([0-9][0-9][0-9][0-9])/, Iyr \1/
s/iyr:[^ ]+/, Iyr 0/
s/eyr:([0-9][0-9][0-9][0-9])/, Eyr \1/
s/eyr:[^ ]+/, Eyr 0/
s/hgt:([0-9]+)cm/, Hgt \1/
s/hgt:([0-9]+)in/, Hgt -\1/
s/hgt:[^ ]+/, Hgt 0/
s/hcl:#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]/, Hcl 1/
s/hcl:[^ ]+/, Hcl 0/
s/ecl:(amb|blu|brn|gry|grn|hzl|oth)/, Ecl 1/
s/ecl:[^ ]+/, Ecl 0/
s/pid:[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]/, Pid 1/
s/pid:[^ ]+/, Pid 0/
s/cid:[^ ]+/, Cid/
s/ ,/,/g
s/^(.)/      \1/