# callback-trait

Implement trait for Fn so that functions that accept traits as parameters can directly receive Fn parameters, such as:

```rust
#[callback_trait]
pub trait ExampleCallback {
	async fn call(&self, p1: u32, p2, u32) -> Result<u32, u32>;
}

fn set_callback(callback: impl ExampleCallback) {
    
}

set_callback(|p1: u32, p2: u32| {
    async move {
        Ok(0)
    }
})

```

