from collections import defaultdict

f = open('2022/stolte/xmas_day7.txt')

data=[]
while True:
    nextline=f.readline()    
    if not nextline:
        break;
    data.append(nextline.strip())
f.close()

fileSystem = defaultdict()
directSize = defaultdict(int)

def getSize(key,directSize,fileSys):
    if directSize[key]==0:
        size = 0
        for i in fileSys[key]:
            if 'dir' in i:
                temp = '$ cd ' + str(i.split(' ')[1])
                x = getSize(temp,directSize,fileSys)
                size += x
            else:
                j=i.split(' ')
                j=int(j[0])
                size+=j                
        return size          
    else:
        return int(directSize[key]);

for i in range(0,len(data)):
    if str(data[i][0])+str(data[i][2])+str(data[i][3]) == '$cd':
        nextLine = data[i+1]
        if nextLine == '$ ls':
            inner=[]
            fileSystem[data[i]]=nextLine
            key = data[i]
            j=i
            while '$ cd' not in nextLine and j < len(data)-2:
                if nextLine != '$ ls':
                    inner.append(nextLine)
                j+=1
                nextLine = data[j+1] 
    #        print("key: " + str(key))
    #        if key in fileSystem:
    #            print("*****key repeat: " + str(key))
            fileSystem[key]=inner

for key in fileSystem:
    directSize[key]=0

for key in directSize:
    directSize[key] = getSize(key,directSize,fileSystem)

dS = defaultdict(int)

for k in directSize:
    if directSize[k]<=100000:
        dS[k]=directSize[k]


count=0
for k in dS:

    count+=dS[k]
                

print(count)
