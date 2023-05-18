import pygame
import os
import sys
import random
import pandas as pd
from itertools import chain

pygame.init()

# Initialize screen and font
screen = pygame.display.set_mode((0, 0), pygame.FULLSCREEN)
pygame.display.set_caption('Experiment')
font = pygame.font.Font(None, 36)

# Load instructions and end messages

instructions = "Welcome! The goal of this task is to test whether you can identify the word that will be presented to you. If you don't have a clear answer, press the \'z\' key to continue to the next picture. As soon as you are confident that you can identify the word that is presented, press the \'m\' key. This will take you to the next set of pictures. There will be three practice trials that are designed to help you get used to the task. Press any key to continue."
practice_end = "Practice trial ended, press any key to continue."
experiment_end = "Experiment ended. Press any key to quit."

exp_path = 'c:\\Users\\stefa\\OneDrive\\Documents\\UNI\\YEAR 3\\II\\Thesis\\experiments\\PRPC\\'

def get_subfolder_image_list(folder_path):
    subfolders = [f.path for f in os.scandir(folder_path) if f.is_dir()]
    random.shuffle(subfolders)
    subfolder_image_files = [[os.path.join(subfolder, img) for img in os.listdir(subfolder)] for subfolder in subfolders]
    return subfolder_image_files

def wrap_text(text, font, max_width):
    words = text.split(' ')
    lines = []
    current_line = ''
    for word in words:
        test_line = current_line + word + ' '
        if font.size(test_line)[0] > max_width:
            lines.append(current_line.strip())
            current_line = word + ' '
        else:
            current_line = test_line
        if '.' in word:
            lines.append(current_line.strip())
            current_line = ''
            lines.append('')
    if current_line:
        lines.append(current_line.strip())
    return lines


def display_message(message, position_y=80, max_width=700):
    screen.fill((255, 255, 255))
    
    if isinstance(message, str):
        wrapped_message = wrap_text(message, font, max_width)
        line_spacing = font.get_linesize()
        for i, line in enumerate(wrapped_message):
            text_surface = font.render(line, True, (0, 0, 0))
            text_rect = text_surface.get_rect(center=(screen.get_width() // 2, position_y + i * line_spacing))
            screen.blit(text_surface, text_rect)
    else:
        screen.blit(message, (screen.get_width() // 2 - message.get_width() // 2, position_y))

    pygame.display.flip()
    wait_for_keypress()
    screen.fill((255, 255, 255)) # Clear the screen after a key is pressed


def wait_for_keypress():
    while True:
        for event in pygame.event.get():
            if event.type == pygame.KEYDOWN:
                return event.unicode
            elif event.type == pygame.QUIT:
                pygame.quit()
                sys.exit()


def main():

    participant_number = input("Enter your participant number: ")
    log_file = f'participant_{participant_number}.csv'

    # Initialize the DataFrame for logging
    columns = ['timestamp', 'event', 'filename']
    log_data = pd.DataFrame(columns=columns)

    display_message(instructions)

    # Practice phase
    practice_folder = 'stimuli_practice'
    practice_subfolders = get_subfolder_image_list(os.path.join(exp_path, practice_folder))
    for subfolder_images in practice_subfolders:
        for image_path in subfolder_images:
            image = pygame.image.load(image_path)
            screen.blit(image, (760, 504))
            pygame.display.flip()
            response = wait_for_keypress()
            screen.fill((255, 255, 255)) # Clear the screen after a key is pressed
            log_data = pd.concat([log_data, pd.DataFrame([{'timestamp': pygame.time.get_ticks(), 'event': response, 'filename': image_path}])], ignore_index=True)
            if response == 'm':
                break

    display_message(practice_end)

    # Experiment phase
    experiment_folder = 'stimuli'
    experiment_subfolders = get_subfolder_image_list(os.path.join(exp_path, experiment_folder))
    for subfolder_images in experiment_subfolders:
        for image_path in subfolder_images:
            image = pygame.image.load(image_path)
            screen.blit(image, (760, 504))
            pygame.display.flip()
            response = wait_for_keypress()
            screen.fill((255, 255, 255)) # Clear the screen after a key is pressed
            log_data = pd.concat([log_data, pd.DataFrame([{'timestamp': pygame.time.get_ticks(), 'event': response, 'filename': image_path}])], ignore_index=True)
            if response == 'm':
                break

    display_message(experiment_end)

    # Save the log_data DataFrame as a CSV file in the current working directory
    os.chdir(exp_path)
    log_data.to_csv(log_file, index=False)
    print(f'Log file saved as: {os.path.abspath(log_file)}') # Print the absolute path of the log file

if __name__ == '__main__':
    main()
    pygame.quit()
