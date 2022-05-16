int f(int x) {
    int g(int y) {
        return x + y;
    }

    int i = 0;
    while (i < 10) {
        int x = 50;
        return g(17);
    }
}

int main() {
	printInt(f(10)); // expected: 27

    return 0;
}
