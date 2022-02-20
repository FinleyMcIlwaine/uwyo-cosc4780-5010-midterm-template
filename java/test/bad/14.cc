int main()
{
    int x = 10;
    {
        int x = x + 1;
        int y = x;
    }
    return y;
}
