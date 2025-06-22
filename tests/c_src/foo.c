int main()
{      
    int sum = 0;
    for (int i = 1; i <= 20; i = i + 1) {
        sum = sum + i;
    }

    sum = sum - 210;
    int j = 1;
    while (j <= 20) {
        sum = sum + j;
        j = j + 1;
    }

    sum = sum - 210;
    int k = 1;
    do {
        sum = sum + k;
        k = k + 1;
    } while (k <= 20);
    

    return sum;
}
