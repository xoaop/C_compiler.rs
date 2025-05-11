

struct Apple {
    apple_idx: i32,
}


fn main() {
    let mut vec = Vec::<Apple>::new();

    println!("{}", vec.len());

    let a1 = Apple {apple_idx: 1};
    // a1.apple_idx = 1;

    println!("{}", a1.apple_idx);

    vec.push(a1);
    println!("{}", vec.len());

    for a in vec {
        println!("{}", vec.len());
        println!("{}", a.apple_idx);
    }
    



}