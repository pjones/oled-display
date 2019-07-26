#include <SPI.h>
#include <Wire.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>
#include <string.h>

#define SCREEN_WIDTH 128 // OLED display width, in pixels
#define SCREEN_HEIGHT 32 // OLED display height, in pixels

// Declaration for an SSD1306 display connected to I2C (SDA, SCL pins)
#define OLED_RESET     4 // Reset pin # (or -1 if sharing Arduino reset pin)
Adafruit_SSD1306 display(SCREEN_WIDTH, SCREEN_HEIGHT, &Wire, OLED_RESET);

/******************************************************************************/
// Current state:
char current_text[256] = "\0";
uint8_t current_text_size = 2;
uint32_t current_flash    = 0;

/******************************************************************************/
void draw(void);
void clear(void);
void reset(void);
void parse_command(uint8_t command);

/******************************************************************************/
void setup() {
  Serial.begin(9600);

  // SSD1306_SWITCHCAPVCC = generate display voltage from 3.3V internally
  if(!display.begin(SSD1306_SWITCHCAPVCC, 0x3C)) { // Address 0x3C for 128x32
    Serial.println(F("SSD1306 allocation failed"));
    for(;;); // Don't proceed, loop forever
  }

  Serial.println("READY");
}

/******************************************************************************/
void loop() {
  if (Serial.available() >= 2) {
    String command = Serial.readStringUntil('\n');
    command.trim();
    parse_command(command);
  }

  if (current_flash > 0) {
    display.clearDisplay();
    display.display();
    delay(current_flash);
  }

  draw();
  if (current_flash > 0) delay(current_flash);
}

/******************************************************************************/
void draw() {
  display.clearDisplay();
  display.setTextSize(current_text_size);
  display.setTextColor(WHITE);
  display.setCursor(0, 0);
  display.cp437(true);

  for (uint8_t i=0; i<sizeof(current_text); ++i) {
    if (current_text[i] == '\0') break;
    display.write(current_text[i]);
  }

  display.display();
}

/******************************************************************************/
void clear() {
  current_text[0] = '\0';
  display.clearDisplay();
  display.display();
}

/******************************************************************************/
void reset() {
  clear();
  current_text_size = 2;
  current_flash = 0;
}

/******************************************************************************/
void parse_command(String command) {
  char command_code = '\0';

  if (command.length() >= 1) {
    command_code = command.charAt(0);
    command = command.substring(1);
  }

  switch (command_code) {
  case 'C': /* Clear the display */
    clear();
    break;

  case 'F': /* Flash the display */
    current_flash = command.toInt();
    break;

  case 'R': /* Reset all options */
    reset();
    break;

  case 'S': /* Set text size */
    {
      int size = command.toInt();

      if (size > 0 && size <= 4)
        current_text_size = size;
    }
    break;

  case 'T': /* Text to diplay */
    {
      size_t n = min(sizeof(current_text), command.length());
      strncpy(current_text, command.c_str(), n);
    }
    break;

  case '\0': /* Unknown command */
  default:
    Serial.println("ERR");
    return;
  }

  Serial.println("ACK");
}
