def partition(array, p, r):
    pivot = array[r]
    i = p
    for j in range(p, r):
        if array[j] < pivot:
            array[i], array[j] = array[j], array[i]
            i += 1
    array[i], array[r] = array[r], array[i]
    return i
