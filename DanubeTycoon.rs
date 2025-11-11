use std::io::{self, Write};
use rand::Rng;

struct Game {
    money: f64,
    lemons: u32,
    sugar: u32,
    cups: u32,
    price_per_cup: f64,
    rebirths: u32,
    bonus_per_sale: f64,
    upgrades: Upgrades,
    day: u32,
    weather: &'static str,
    raccoon_event: bool,
}

struct Upgrades {
    better_lemon_quality: bool,
    better_sugar_quality: bool,
    better_cups: bool,
    mega_lemon: bool,
    super_cup: bool,
    automatic_sales: bool,
}

impl Game {
    fn new() -> Self {
        let mut game = Self {
            money: 10.0,
            lemons: 0,
            sugar: 0,
            cups: 0,
            price_per_cup: 1.0,
            rebirths: 0,
            bonus_per_sale: 0.0,
            upgrades: Upgrades {
                better_lemon_quality: false,
                better_sugar_quality: false,
                better_cups: false,
                mega_lemon: false,
                super_cup: false,
                automatic_sales: false,
            },
            day: 1,
            weather: "Sunny",
            raccoon_event: false,
        };
        game.next_day(); // initialize weather
        game
    }

    fn run(&mut self) {
        loop {
            self.draw();
            println!("Choose an action:");
            println!("1) Buy lemons ($1 each)");
            println!("2) Buy sugar ($0.5 each)");
            println!("3) Buy cups ($0.1 each)");
            println!("4) Set price per cup");
            println!("5) Sell lemonade");
            println!("6) Buy Danube Pizza ($97,600,000 each) [Rebirth]");
            println!("7) Buy Upgrades");
            println!("8) Next Day");
            println!("9) Quit");

            let choice = self.get_input();
            match choice.as_str() {
                "1" => self.buy_lemons(),
                "2" => self.buy_sugar(),
                "3" => self.buy_cups(),
                "4" => self.set_price(),
                "5" => self.sell(),
                "6" => self.buy_pizza(),
                "7" => self.buy_upgrades(),
                "8" => self.next_day(),
                "9" => {
                    println!("Thanks for playing!");
                    break;
                }
                _ => println!("Invalid choice!"),
            }
        }
    }

    fn draw(&self) {
        println!("==============================");
        println!("🍋 Lemonade Tycoon 🍋 - Day {}", self.day);
        println!("Money: ${:.2}", self.money);
        println!("Lemons: {}", self.lemons);
        println!("Sugar: {}", self.sugar);
        println!("Cups: {}", self.cups);
        println!("Price per cup: ${:.2}", self.price_per_cup);
        println!("Rebirths: {}", self.rebirths);
        println!("Bonus per sale: ${:.2}", self.bonus_per_sale);
        println!("Weather: {}", self.weather);
        if self.raccoon_event {
            println!("🦝 A wild raccoon is messing with your stand!");
        }
        println!("Upgrades:");
        println!(
            "Better lemons: {}, Better sugar: {}, Better cups: {}",
            self.upgrades.better_lemon_quality,
            self.upgrades.better_sugar_quality,
            self.upgrades.better_cups
        );
        println!(
            "Mega Lemon: {}, Super Cup: {}, Automatic Sales: {}",
            self.upgrades.mega_lemon,
            self.upgrades.super_cup,
            self.upgrades.automatic_sales
        );
        println!("==============================");
    }

    fn get_input(&self) -> String {
        let mut input = String::new();
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut input).unwrap();
        input.trim().to_string()
    }

    fn buy_lemons(&mut self) {
        println!("How many lemons to buy?");
        let amount = self.get_input().parse::<u32>().unwrap_or(0);
        let cost = amount as f64 * 1.0;
        if self.money >= cost {
            self.money -= cost;
            self.lemons += amount;
            println!("Bought {} lemons.", amount);
        } else {
            println!("Not enough money!");
        }
    }

    fn buy_sugar(&mut self) {
        println!("How much sugar to buy?");
        let amount = self.get_input().parse::<u32>().unwrap_or(0);
        let cost = amount as f64 * 0.5;
        if self.money >= cost {
            self.money -= cost;
            self.sugar += amount;
            println!("Bought {} sugar.", amount);
        } else {
            println!("Not enough money!");
        }
    }

    fn buy_cups(&mut self) {
        println!("How many cups to buy?");
        let amount = self.get_input().parse::<u32>().unwrap_or(0);
        let cost = amount as f64 * 0.1;
        if self.money >= cost {
            self.money -= cost;
            self.cups += amount;
            println!("Bought {} cups.", amount);
        } else {
            println!("Not enough money!");
        }
    }

    fn set_price(&mut self) {
        println!("Set price per cup:");
        let price = self.get_input().parse::<f64>().unwrap_or(self.price_per_cup);
        if price > 0.0 {
            self.price_per_cup = price;
            println!("Price set to ${:.2} per cup.", price);
        } else {
            println!("Invalid price!");
        }
    }

    fn sell(&mut self) {
        let cups_sold = self.lemons.min(self.sugar).min(self.cups);
        if cups_sold == 0 {
            println!("Not enough ingredients to make lemonade!");
            return;
        }

        let mut multiplier = match self.weather {
            "Sunny" => 1.5,
            "Cloudy" => 1.0,
            "Rainy" => 0.7,
            _ => 1.0,
        };

        if self.raccoon_event {
            multiplier *= 0.8; // raccoon reduces sales
        }

        // Upgrade bonuses
        if self.upgrades.mega_lemon {
            multiplier += 0.2;
        }
        if self.upgrades.super_cup {
            multiplier += 0.1;
        }

        let earnings = cups_sold as f64 * (self.price_per_cup + self.bonus_per_sale) * multiplier;
        self.money += earnings;
        self.lemons -= cups_sold;
        self.sugar -= cups_sold;
        self.cups -= cups_sold;

        println!("Sold {} cups for ${:.2}!", cups_sold, earnings);
    }

    fn buy_pizza(&mut self) {
        let price = 97_600_000.0;
        if self.money >= price {
            self.money -= price;
            self.rebirths += 1;
            self.lemons = 0;
            self.sugar = 0;
            self.cups = 0;
            self.price_per_cup = 1.0;
            self.bonus_per_sale += 0.5;
            println!("🎉 You bought a Danube Pizza! Rebirth #{} achieved! Bonus per sale increased!", self.rebirths);
        } else {
            println!("Not enough money for Danube Pizza!");
        }
    }

    fn buy_upgrades(&mut self) {
        println!("Choose an upgrade:");
        println!("1) Better lemon quality ($50)");
        println!("2) Better sugar quality ($50)");
        println!("3) Better cups ($50)");
        if self.rebirths >= 1 {
            println!("4) Mega Lemon ($100) [Unlocked after 1 rebirth]");
        }
        if self.rebirths >= 2 {
            println!("5) Super Cup ($150) [Unlocked after 2 rebirths]");
        }
        if self.rebirths >= 3 {
            println!("6) Automatic Sales ($500) [Unlocked after 3 rebirths]");
        }

        let choice = self.get_input();
        match choice.as_str() {
            "1" => self.purchase_upgrade(&mut self.upgrades.better_lemon_quality, 50, "Better lemon quality"),
            "2" => self.purchase_upgrade(&mut self.upgrades.better_sugar_quality, 50, "Better sugar quality"),
            "3" => self.purchase_upgrade(&mut self.upgrades.better_cups, 50, "Better cups"),
            "4" if self.rebirths >= 1 => self.purchase_upgrade(&mut self.upgrades.mega_lemon, 100, "Mega Lemon"),
            "5" if self.rebirths >= 2 => self.purchase_upgrade(&mut self.upgrades.super_cup, 150, "Super Cup"),
            "6" if self.rebirths >= 3 => self.purchase_upgrade(&mut self.upgrades.automatic_sales, 500, "Automatic Sales"),
            _ => println!("Invalid choice or upgrade locked!"),
        }
    }

    fn purchase_upgrade(&mut self, upgrade: &mut bool, cost: f64, name: &str) {
        if *upgrade {
            println!("You already own {}!", name);
            return;
        }
        if self.money >= cost {
            self.money -= cost;
            *upgrade = true;
            println!("{} purchased!", name);
        } else {
            println!("Not enough money for {}!", name);
        }
    }

    fn next_day(&mut self) {
        self.day += 1;
        self.weather = self.generate_weather();
        self.raccoon_event = self.check_raccoon_event();
        println!("➡️ Moving to day {}...", self.day);

        if self.upgrades.automatic_sales {
            println!("💸 Automatic sales active! Selling lemonade automatically...");
            self.sell();
        }
    }

    fn generate_weather(&self) -> &'static str {
        let r = rand::thread_rng().gen_range(0..100);
        match r {
            0..=60 => "Sunny",
            61..=85 => "Cloudy",
            _ => "Rainy",
        }
    }

    fn check_raccoon_event(&self) -> bool {
        let r = rand::thread_rng().gen_range(0..100);
        r < 15 // 15% chance of raccoon
    }
}

fn main() {
    let mut game = Game::new();
    game.run();
}
