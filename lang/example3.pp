/*
int x = 69;
string y = "haha";
bool z = true;

int x2 = (-2) * x + 2 - 1 + (4 / 2) - (3 % 2);

bool z2 = (true || false) && (!false || true);
*/
// ========================================================================================
/*
int a = 2;
int f(int a, int b) {
    int f2(int a) {
        int a = 3;

        return a;
    }

    return 3;
}


int main() {
    int x = 2;
}
*/
// ========================================================================================

int f(int a) {
    int b;

    int g(int x, int y) {
        string s;

        return 1;
    }

    return g(1, 20);
}

int main() {
    // a++; // error
    // b++; // error
    int temp = 1;
    temp = temp + f(2);
    printInt(temp);

    // x++; // error
    // y++; // error
    // printString(s); // error
    // g(1, 2); // error

    // no-return error
}

// ========================================================================================
