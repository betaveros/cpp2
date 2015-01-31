# CPP^2

(C Plus Plus Competitive Programming Preprocessor)

Sick of all of this `FOR(i,0,n)` nonsense and `scand(x,y,z,w)` and that fifty-line sequence of `#include`s and debug macros at the front of every one of your programs?

I was too, so I wrote this.

Now, just write
```
vector int v;
int main() {
    s! int n;
    sn! v, n;
    sort! v;
    for i to n {
        int y <~ v;
        pn! y;
    }
}
```
and automatically get
```cpp
#include <algorithm>
#include <cstdio>
#include <vector>
using namespace std;
vector<int> v;
int main() {
    int n; scanf("%d", &n);
    for (int _t0 = n; _t0; --_t0) { int _t1; scanf("%d", &_t1); v.push_back(_t1); }
    sort(v.begin(), v.end());
    for (int i = 1; i <= n; ++i){
        int y = v.back(); v.pop_back();
        printf("%d\n", y);
    }
}
```

## FAQ (Fictionally Asked Questions)

### Why don't you just learn a new language?

I do and I have. I love learning languages. But until USACO, TopCoder, and a large number of online judges start accepting solutions in Haskell or D, there doesn't seem to be any alternative.

(Besides, which of these languages has a built-in dedicated operator for adding two numbers and taking the result modulo 1000000007?)

### Why don't you write a complete language that compiles into C++?

I considered that, but ultimately decided the effort wouldn't be worth it. C++ has a huge grammar and an amazing number of features that are useful in 10% or 1% of problems and there's no way I'm writing code to parse all of them to spit it out again.

### What's with the weird operator symbols?

The min, max, minify, and maxify operators (`<?`, `>?`, `<?=`, `>?=`) are taken from LiveScript.

The `(push|pop)_(front|back)` operators (`~=`, `<~`, `~~=`, `<~~`) are mostly my own invention. I wanted to take some Scala-inspired operators (`:+=` and derivatives, with the mnemonic the COLon is on the COLlection side) but it turns out using the colon screws with C indentation, so I took the path of least resistance and chose something else. `~=` is the operator as it's used in D, as `push_back`. LiveScript has curly arrows for a completely different purpose, but at least that justifies that it looks like an arrow.

### Why doesn't sugary `for` cache the range it iterates over in some temporary variable? That's expensive.

I wanted it to translate to natural C++ and not break any obfuscation rules. When you need to avoid recomputing something expensive, stash it in a local variable.

### The macros / the special operators / the type inference / ALL the things are broken!

That's not a question, but I am not surprised because it's a huge collection of hacks. Still, it works 90% of the time and you can (in theory) always lapse into vanilla C++.

The sugar operators are entirely the same precedence as assignment operators and entirely right-associative because parsing is hard. You shouldn't need to chain too many sugar operators anyway. If you do, adding parentheses helps.

There is no type inference system. What I call the *type-guessing* system is one of the hackiest things ever. If you stash your expression in a local variable with a declared type, you can help the guesser out.
