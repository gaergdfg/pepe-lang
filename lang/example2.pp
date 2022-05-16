int f(int a, int b) {
	if (a != b) {
        a++;
        b = f(a, b);
    }

	return a + b;
}

int main() {
	int b = 5;
	int a = f(2, 5);
	
	return a;
}
