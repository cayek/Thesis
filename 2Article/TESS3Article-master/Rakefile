require 'colorize'

namespace :overleaf do

  desc "Pull overleaf git repository"
  task :pull do
    # pull
    puts "pull overleaf".green
    Dir.chdir("ArticleOverleaf") do
      sh "git pull"
    end
    # test if no changes detected in Article
    out = `git status Article`
    if !(out.include? "nothing to commit, working directory clean")
      puts("Article dir must be cleen".red)
      return
    end
    puts "copy ArticleOverleaf to Article".green
    sh 'rsync -avz --exclude=".git/" ArticleOverleaf/ Article/'
  end
end
