int main()
{
    int x = 1;
    if (x == 1)
    {
        return 3;
    } 
    else 
    {
        x = 2;
    }
    for ({ int x = 1; } x < 10; x++ )
    {
        return x;
    }
    return x;
}