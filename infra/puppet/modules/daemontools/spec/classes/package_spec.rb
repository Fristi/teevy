require 'spec_helper'

describe 'daemontools::package' do
  it { should contain_package('daemontools') }
  it { should contain_package('daemontools-run') }
  it { should contain_exec('daemontools-start') }
end
