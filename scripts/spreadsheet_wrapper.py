#!/usr/bin/env python3
import sys
import gspread
from oauth2client.service_account import ServiceAccountCredentials

# Usage:
# script.py spreadsheet_id command [arguments]

def unsub(wks, username):
  try:
    # Find all the cells, where username is mentioned
    cells = wks.findall(username)
  except gspread.exceptions.CellNotFound:
    # If there's no cells with the username, then there's no need to do anything
    return
  else:
    # Empty those cells
    # gspread doesn't provide any way to remove a row in a spreadsheet, so...
    for cell in cells:
      cell.value = ""
    wks.update_cells(cells)

def print_list(wks):
  # Filtering empty lines and removing the header
  users = list(filter(lambda x:x!="",  wks.col_values(2)[1:]))
  for user in users:
    print(user)

def main():
  # Boring authenthication process
  scope = ['https://spreadsheets.google.com/feeds']
  credentials = ServiceAccountCredentials.from_json_keyfile_name('google.json', scope)
  gc = gspread.authorize(credentials)

  wks = gc.open_by_key(sys.argv[1]).sheet1
  if sys.argv[2] == "unsubscribe":
    unsub(wks, sys.argv[3])
  elif sys.argv[2] == "get_subs":
    print_list(wks)
  else:
    raise ValueError("UnknownCommand")

if __name__ == "__main__":
  try:
    main()
  except:
    print ("Unexpected error:", sys.exc_info()[0], file=sys.stderr)
    sys.exit(1)
  sys.exit(0)
