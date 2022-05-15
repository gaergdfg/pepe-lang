void greet() {
	printString("Hello world");

	return;
}

int a(int x) {
	int b(int val) {
		return 2 * val;
	}

	return b(x);
}

int fib(int n) {
	if (n < 2) {
		return 1;
	}

	return fib(n - 1) + fib(n - 2);
}

void inc(ref int val) {
	val++;

	return;
}

void main() {
	greet();

	int x;
	x = 2 + 2;

	printBool(x > 3 - 1);
	printBool(2 * 2 < 3 / 1);

	int i = 0;
	int n = 10;
	int dummy;
	while (i < n) {
		printInt(a(i));
		i++;
	}

	printInt(fib(5));

	int y = 1;
	inc(y);
	printInt(y);

	return;
}
