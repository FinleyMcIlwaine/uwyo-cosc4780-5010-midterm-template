int factr(int n)
{
    if (n < 2)
        return 1;
    else
        return (n * factr(n - 1));
    return 0;
}

int main()
{
    int i = factr(7);
    return 0;
}