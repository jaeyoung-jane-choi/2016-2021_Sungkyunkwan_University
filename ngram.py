# 자연어처리과제_1 Ngram 함수
# 최재영 문헌정보학과 2016312411


def n_gram(text, n):
    """
    :param text: str
    :param n: int (shorter than length_division of text)
    :return: list (of str)
    """
    # split the string text into a list
    textlist = text.split()
    #length variable
    length = len(textlist)
    #newlist to append
    newlist= []

    if n == 0: #if 0, do not split, put it in one list
        newlist.append(text)
        return newlist

    elif n == 1:
        return textlist

    else:
        for i in range(length-1):# i index is 0 ... length-2
            if i+n <= length : #if index is over length stop process
                newtext = textlist[i:i+n] #slices to a list
                newtext = ' '.join(newtext) #join list element into a string
                newlist.append(newtext) #append elements to a newlist
            else:
                pass

        return newlist


# example

stext = "Colaboratory(또는 줄여서 'Colab')를 사용하면 브라우저에서 Python을 작성하고 실행할 수 있습니다."
text = '워너브라더스에서는 DCEU(DC 확장 유니버스)와 상관없는 조커 영화가 하나 더 제작된다.'

#print(n_gram(text,0))
#print(n_gram(stext,1))
#print(n_gram(text,2))
print(n_gram(stext,4))


