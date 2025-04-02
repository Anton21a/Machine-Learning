#!/bin/bash

cd ~/Desktop/MA1y/Prediction_with_ML/Machine-Learning || { echo "Папка не найдена"; exit 1; }

git pull origin main

git add .

commit_message=${1:-"Update"}
git commit -m "$commit_message"

git push origin main
