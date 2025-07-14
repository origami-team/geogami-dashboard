# GEOGAMI DASHBOARD

<img src="https://geogami.ifgi.de/pictures/logo/icon.png" alt="Logo" width="60">

## 📑 Table of contents
1. [Project presentation](#project-presentation)
2. [Functionalities](#functionalities)
3. [Tutorial](#tutorial)
    * [Sidebar](#sidebar)
    * [Main panel](#main-panel)

## Project presentation
Welcome to the GeoGami dashboard ! The goal of the dashboard is to analyse your results after you play a game in GeoGami. Now you are able to see in details all information on your tasks.

## Functionalities
- 📌 Sum up your game in a table with all the key information.
- 🗺️ See your trajectory and your answers on the map.
- ✅ Check your answers, for example with pictures taken during the game.
- 💡 Compare statistics with tables and graphics between different players.
- 📤 Save all visualisations.

## Tutorial
When you open the GeoGami Dashboard, you can see two things : sidebar (on the left) - Main panel (on the right)

### Sidebar
1. **Choose theme**
    * Light or dark theme
2. **Select your game**
    * Here you can get the list of all the games that have been made and played on GeoGami. 
    * 'Type' the name of your game and select it in this field.
3. **Select the players**
    * Here you can see the list of GeoJSON files that match your game selection in the filter above. Initially, they are not loaded and are only displayed for the selection. Enter the name of the files you want to load. It is important to load the files here so that you can see the analysis in the tabs of the main panel (on the right).
    * 'Reset Selection' button: this resets all the 'selected files' you chose, only in this field.
4. **Enter the task number:**
    * Here, you can provide the 'task number' of the tasks present in GeoJSON file. This is an important field and helps to show visualizations in the main panel. In particular, in the map, pictures, 'All of your plays' and 'Statistics Per Task' (Note: they are discussed in 'Main Panel' section below).

### Main panel
**General overview:** We have 3 tabs in the main panel to get the general overview, for a single file, they are as follows :
1. **All of your tasks**
    * Here, all the files that you loaded in 'select the players' field under the 'sidebar panel', are present. You can individually analyse each file by selecting them from the 'choose file to view data' filter.
    * Below this, you can see 'overall score'. This is the general score that was given as per all the correct answers earned by the participant.
    * Afterwards, you get the whole 'Big table' for the general analysis. You can also save this big table using the 'save to csv' button.
2. **Maps**
    * Here, the map that you see by default, is affected by 2 fields, the 'file' that is currently chosen by you in 'choose file to view data' of this map tab and also, the 'task number' that you chose in 'Enter a task number'. You can change the task number (in the sidebar panel) and select another file in this 'map' tab, to see the required trajectory/interaction of the player on the map.
    * Note: only those files are visible here that you loaded in 'select the players' field in the sidebar panel.
    * You can also download these maps using the 'save the map' button present below

3. **Pictures**
    * This tab is specifically made to analyse the 'picture task' Here, you can select the file from the field 'choose file to view data' to view the picture taken by the user for the 'picture task'.
    * Note: only those files are visible here that you loaded in 'select the players' field in the sidebar panel.

**Tabs for comparison:** We have two comparisons tabs in the main panel, they are as follows:

4. **All of your plays**
    * Here, you first choose multiple files that you want to compare in the 'Choose files to compare' field.
    * Note: only those files are visible here that you loaded in 'select the players' field in the sidebar panel.
    * Thereafter, you get a nice comparison view. You can change the 'Task number' on the sidebar panel, to compare that particular task for both the players.
    * 'Save to CSV' button can be used to download the comparison analysis.

5. **Statistics per task**
    * Here, you first choose multiple files that you want to compare in the 'Choose files to compare' field. 
    * Note: only those files are visible here that you loaded in 'select the players' field in the sidebar panel.
    * After that, another option that you have is the 'Choose Graphic to display', you can choose it as per your requirement.
    * You can change the 'Task number' on the sidebar panel, to compare that particular task for both the players.
    * In the end, you get the possible visualizations.
    * You can save these visualizations using the 'Save to png' button present in the bottom left.
