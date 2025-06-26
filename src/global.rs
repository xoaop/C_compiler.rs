use std::collections::HashMap;

pub struct VariableContext {
    name_map: HashMap<String, i32>,
    loop_id_counter: i32,
}

impl VariableContext {
    pub fn new() -> Self {
        VariableContext {
            name_map: HashMap::<String, i32>::new(),
            loop_id_counter: 0,
        }
    }

    pub fn make_temporary(&mut self, prefix: Option<String>) -> String {
        let prefix = prefix.unwrap_or_else(|| "tmp".to_string());

        if self.name_map.contains_key(&prefix) {
            let counter = self.name_map.get_mut(&prefix).unwrap();
            *counter += 1;
        } else {
            self.name_map.insert(prefix.clone(), 0);
        }

        return format!("{}.{}", prefix, self.name_map.get(&prefix).unwrap());
    }

    pub fn record_loop_id(&mut self) -> i32 {
        let new_loop_id = self.loop_id_counter;
        self.loop_id_counter = self.loop_id_counter + 1;
        return new_loop_id;
    }
}


